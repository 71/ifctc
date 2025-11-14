//! Analyzes a code base to find missing changes.
const Analyzer = @This();

pub const Diagnostic = @import("Diagnostic.zig");
pub const Dir = @import("Dir.zig");

const ChangeSet = @import("ChangeSet.zig");
const CommentSet = @import("CommentSet.zig");
const DirectiveParser = @import("DirectiveParser.zig");
const std = @import("std");
const strings = @import("strings.zig");
const chunk_list = @import("chunk_list.zig");

/// A file in the code base.
pub const File = struct {
    const Label = struct {
        line: u32,
        status: enum { changed, unchanged },
    };

    /// Information about what changed in this file.
    change: *const ChangeSet.File,

    /// The allocator of the worker which processed the file.
    worker_allocator: ?*std.heap.ArenaAllocator = null,

    /// Whether the file is done being processed.
    done: std.atomic.Value(bool) = .init(false),

    /// The list of diagnostics found in this file.
    diagnostics: chunk_list.ChunkList(Diagnostic, 16) = .empty,

    /// The map of labels defined in this file. Should only be accessed by another threads if
    /// `done.load(.acquire)` is true.
    labels: std.hash_map.StringHashMapUnmanaged(Label) = .empty,

    pub fn deinit(self: *File) void {
        const worker_allocator = self.worker_allocator orelse {
            std.debug.assert(self.diagnostics.isEmpty());
            std.debug.assert(self.labels.count() == 0);
            return;
        };

        self.diagnostics.deinit(worker_allocator.allocator());
        self.labels.deinit(worker_allocator.allocator());
        self.* = undefined;
    }

    pub fn fixupDiagnostics(self: *File, files: []const File) usize {
        var iter = self.diagnostics.iterator();
        var count: usize = 0;
        while (iter.next()) |diagnostic| {
            switch (diagnostic.details) {
                .label_status_not_yet_available => |info| {
                    const file = &files[info.file_id];

                    if (file.labels.get(info.label)) |label| {
                        switch (label.status) {
                            .changed => continue,
                            .unchanged => if (info.changed_line) |changed_line| {
                                diagnostic.details = .{
                                    .label_not_modified = .{ .label = info.label, .then_change_line = diagnostic.line },
                                };
                                diagnostic.line = changed_line;
                            } else continue,
                        }
                    } else {
                        diagnostic.details = .{ .label_does_not_exist = info.label };
                    }
                },
                else => {},
            }
            count += 1;
        }
        return count;
    }

    pub fn copyDiagnostics(
        self: *const File,
        allocator: std.mem.Allocator,
        len: usize,
    ) error{OutOfMemory}![]Diagnostic {
        if (len == 0) return &[0]Diagnostic{};

        var result = try allocator.alloc(Diagnostic, len);
        var i: usize = 0;

        errdefer {
            // Cannot free individual diagnostics.
            allocator.free(result);
        }

        var iter = self.diagnostics.constIterator();
        while (iter.next()) |diagnostic| {
            if (diagnostic.details == .label_status_not_yet_available) continue;

            result[i] = try diagnostic.copy(allocator);
            i += 1;
        }
        std.debug.assert(i == len);
        return result;
    }

    /// Sets `done` to true, allowing future calls to `labelsIfDone()` to return the labels.
    pub fn setDone(self: *File, worker: *Worker) void {
        self.worker_allocator = worker.allocator;
        self.done.store(true, .release);
    }

    /// Returns the labels if `done` is true, or null otherwise.
    pub fn labelsIfDone(self: *const File) ?*const std.hash_map.StringHashMapUnmanaged(Label) {
        return if (self.done.load(.acquire)) &self.labels else null;
    }
};

allocator: std.mem.Allocator,
files: []File,
file_by_path: std.StringHashMapUnmanaged(*const File),
next_file_index: std.atomic.Value(usize) = .init(0),

root_directory: *const Dir,

pub fn init(
    allocator: std.mem.Allocator,
    changes: *const ChangeSet,
    directory: *const Dir,
) error{OutOfMemory}!Analyzer {
    const change_files = changes.files.items;
    const files = try allocator.alloc(File, change_files.len);
    errdefer allocator.free(files);

    var files_map: std.StringHashMapUnmanaged(*const File) = .empty;
    errdefer files_map.deinit(allocator);
    try files_map.ensureTotalCapacity(allocator, @intCast(change_files.len));

    for (change_files, 0..) |*change, i| {
        files[i] = .{ .change = change };

        try files_map.put(allocator, change.path, &files[i]);
    }

    return .{
        .allocator = allocator,
        .files = files,
        .file_by_path = files_map,
        .root_directory = directory,
    };
}

pub fn deinit(self: *Analyzer) void {
    for (self.files) |*file| {
        file.deinit();
    }
    self.file_by_path.deinit(self.allocator);
    self.allocator.free(self.files);
}

/// Analyzes all pending source files until done. This can be called from multiple threads at once.
pub fn analyzeAll(
    self: *const Analyzer,
    worker_allocator: *std.heap.ArenaAllocator,
    per_file_allocator: *std.heap.ArenaAllocator,
) error{OutOfMemory}!void {
    var worker: Worker = .{
        .analyzer = self,
        .allocator = worker_allocator,
        .per_file_allocator = per_file_allocator,
    };

    while (try worker.analyzeOne()) {}
}

// -------------------------------------------------------------------------------------------------
// MARK: Worker

const Worker = struct {
    /// The shared analyzer.
    analyzer: *const Analyzer,

    /// Allocator used for allocations whose lifetime matches that of the `Worker`.
    allocator: *std.heap.ArenaAllocator,
    /// Allocator used for allocations whose lifetime match that of a single file being analyzed. It
    /// is reset after each file is processed.
    per_file_allocator: *std.heap.ArenaAllocator,

    file_id: u32 = 0,
    file: *File = undefined,

    /// The directory name of the `file`.
    file_dir: []const u8 = undefined,

    if_change_label: std.ArrayList(u8) = .empty,
    last_directive: union(enum) { none, if_change_line: u32, then_change_line: u32 } = .none,

    /// Analyzes one pending source file, returning true if there was a pending file, and false if all
    /// files have been analyzed.
    fn analyzeOne(self: *Worker) error{OutOfMemory}!bool {
        _ = self.per_file_allocator.reset(.retain_capacity);

        const next_file_index_ptr: *std.atomic.Value(usize) = @constCast(&self.analyzer.next_file_index);
        const index = next_file_index_ptr.fetchAdd(1, .acq_rel);

        if (index >= self.analyzer.files.len) {
            return false;
        }

        const file = &self.analyzer.files[index];
        const file_path = file.change.path;
        const file_dir_end = std.mem.lastIndexOfScalar(u8, file_path, '/') orelse 0;

        self.file = @constCast(file);
        self.file_id = @intCast(index);

        defer self.file.setDone(self);

        switch (file.change.status) {
            .binary, .deleted, .renamed_to => return true, // Do not attempt to analyze the file.
            .new, .modified_lines, .modified_ranges => {},
        }

        var file_reader = self.analyzer.root_directory.openFile(file_path) catch |err| {
            try self.addDiagnostic(0, .{ .cannot_open = err });
            return true;
        };
        defer file_reader.close();

        // We're the only ones with access to this file (obtained with `fetchAdd()` above), so we
        // can mutate it.
        self.last_directive = .none;
        self.if_change_label.clearRetainingCapacity();
        self.file_dir = file_path[0..file_dir_end];

        var parser: DirectiveParser = .init(self.per_file_allocator.allocator(), CommentSet.forExtension(file_path));
        defer parser.deinit();

        var expect_more: bool = false;
        var buffer: [4096]u8 = undefined;

        while (true) {
            const read = file_reader.read(&buffer) catch |err| {
                try self.addDiagnostic(0, .{ .cannot_read = err });
                return true;
            };
            if (read == 0) break;

            // Process the buffer.
            expect_more = try self.analyzeBuffer(&parser, &buffer);
        }

        if (expect_more) {
            try self.addDiagnostic(0, .invalid_directive);
        } else {
            switch (self.last_directive) {
                .none, .then_change_line => {},
                .if_change_line => |line| {
                    try self.addDiagnostic(line, .if_change_without_then_change);
                },
            }
        }
        return true;
    }

    fn analyzeBuffer(self: *Worker, parser: *DirectiveParser, buffer: []const u8) error{OutOfMemory}!bool {
        var buf = buffer;

        while (true) {
            const directive = parser.next(&buf) catch |err| switch (err) {
                error.WantMore => {
                    // Keep reading.
                    return true;
                },
                error.InvalidDirective => {
                    try self.addDiagnostic(parser.line, .invalid_directive);
                    continue;
                },
                error.OutOfMemory => return error.OutOfMemory,
            } orelse {
                // Nothing in this buffer.
                return false;
            };

            switch (self.last_directive) {
                .none => if (directive.directive_ty == .then_change) {
                    try self.addDiagnostic(directive.start_line, .then_change_without_if_change);
                    self.last_directive = .{ .then_change_line = directive.start_line };
                } else {
                    self.last_directive = .{ .if_change_line = directive.start_line };
                    try self.setIfChangeLabel(directive.start_line, directive.args);
                },
                .if_change_line => |line| if (directive.directive_ty == .if_change) {
                    try self.addDiagnostic(directive.start_line, .{
                        .if_change_follows_if_change = .{
                            .if_change_line = line,
                        },
                    });
                    self.last_directive = .{ .if_change_line = directive.start_line };
                } else {
                    self.last_directive = .{ .then_change_line = directive.start_line };
                    try self.processThenChange(line, directive.start_line, &directive);
                },
                .then_change_line => |line| if (directive.directive_ty == .then_change) {
                    try self.addDiagnostic(directive.start_line, .{
                        .then_change_follows_then_change = .{
                            .then_change_line = line,
                        },
                    });
                    self.last_directive = .{ .then_change_line = directive.start_line };
                } else {
                    self.last_directive = .{ .if_change_line = line };
                    try self.setIfChangeLabel(directive.start_line, directive.args);
                },
            }
        }
        return false;
    }

    fn processThenChange(
        self: *Worker,
        start_line: u32,
        end_line: u32,
        directive: *const DirectiveParser.ParsedDirective,
    ) error{OutOfMemory}!void {
        const modified_line = self.file.change.firstModifiedLineIn(start_line, end_line);

        const label = self.if_change_label.items;
        defer self.if_change_label.clearRetainingCapacity();

        if (label.len > 0) {
            try self.markLabelChanged(label, start_line, modified_line != null);
        }

        if (directive.args.len == 0) {
            return self.addDiagnostic(start_line, .invalid_directive);
        }

        // Check arguments.
        for (directive.args) |path| {
            try self.checkPathChanged(modified_line, end_line, path);
        }
    }

    fn markLabelChanged(
        self: *Worker,
        label: []const u8,
        line: u32,
        modified: bool,
    ) error{OutOfMemory}!void {
        const label_entry = try self.file.labels.getOrPut(self.allocator.allocator(), label);
        if (label_entry.found_existing) {
            return self.addDiagnostic(line, .{
                .duplicate_label = .{
                    .label = try self.allocator.allocator().dupe(u8, label),
                    .previous_line = label_entry.value_ptr.line,
                },
            });
        }
        errdefer self.file.labels.removeByPtr(label_entry.key_ptr);

        // We looked up label with an unowned slice; copy it here.
        label_entry.key_ptr.* = try self.allocator.allocator().dupe(u8, label);
        label_entry.value_ptr.line = line;
        label_entry.value_ptr.status = if (modified) .changed else .unchanged;
    }

    /// Sets the `if_change_label` field, reporting diagnostics if `args.len > 1`.
    fn setIfChangeLabel(self: *Worker, line: u32, args: []const []const u8) error{OutOfMemory}!void {
        std.debug.assert(self.if_change_label.items.len == 0);

        switch (args.len) {
            0 => {},
            1 => try self.if_change_label.appendSlice(self.allocator.allocator(), args[0]),
            else => {
                @branchHint(.unlikely);
                try self.addDiagnostic(line, .invalid_directive);
            },
        }
    }

    /// Checks whether the file at the given path changed, reporting diagnostics as needed.
    fn checkPathChanged(
        self: *Worker,
        changed_line: ?u32,
        then_change_line: u32,
        path_and_label: []const u8,
    ) error{OutOfMemory}!void {
        // Extract the label out of the path.
        var raw_path = path_and_label;
        var label: ?[]const u8 = null;

        if (std.mem.lastIndexOfAny(u8, path_and_label, "/:")) |index| {
            if (index == path_and_label.len - 1) {
                return self.addDiagnostic(then_change_line, .{
                    .invalid_path = try self.allocator.allocator().dupe(u8, path_and_label),
                });
            }

            if (path_and_label[index] == ':') {
                raw_path = path_and_label[0..index];
                label = path_and_label[index + 1 ..];
            }
        }

        if (raw_path.len == 0) {
            std.debug.assert(label != null and label.?.len > 0);

            return try self.checkFileChanged(changed_line, then_change_line, self.file, label);
        }

        // Resolve the path.
        const resolved_path = try self.resolvePath(raw_path, then_change_line) orelse return;
        const context: ConcatStringContext = .{};

        // Find the corresponding file.
        const file = self.analyzer.file_by_path.getAdapted(resolved_path, context) orelse {
            const file_path = try std.mem.concat(self.allocator.allocator(), u8, &resolved_path);
            const file_exists = self.analyzer.root_directory.fileExists(file_path);
            if (changed_line == null and file_exists) return;

            const details: Diagnostic.Details = if (file_exists)
                .{
                    .file_not_modified = .{ .path = file_path, .then_change_line = then_change_line },
                }
            else
                .{ .file_not_found = file_path };
            return self.addDiagnostic(changed_line orelse then_change_line, details);
        };

        // Check the file.
        try self.checkFileChanged(changed_line, then_change_line, file, label);
    }

    /// Checks whether the given file / label changed, reporting diagnostics as needed.
    fn checkFileChanged(
        self: *Worker,
        changed_line: ?u32,
        then_change_line: u32,
        file: *const File,
        given_label: ?[]const u8,
    ) error{OutOfMemory}!void {
        if (file == self.file) {
            // Prevent a `ThenChange` from only depending on changes in its own file / label.
            if (given_label) |label| {
                if (std.mem.eql(u8, self.if_change_label.items, label)) {
                    return self.addDiagnostic(then_change_line, .{
                        .self_label = .{ .label = try self.allocator.allocator().dupe(u8, label) },
                    });
                }
            } else {
                return self.addDiagnostic(then_change_line, .self_file);
            }
        }

        const file_id: u32 = @intCast(file - self.analyzer.files.ptr);
        var current_file_id = file_id;

        sw: switch (file.change.status) {
            .new => {
                // If the file is new, then all of its content changed.
                return;
            },
            .binary => {
                // If the file is binary, then it can't have labels.
                if (given_label == null) {
                    return;
                }
                return self.addDiagnostic(then_change_line, .{
                    .binary_file_cannot_have_labels = file_id,
                });
            },
            .deleted => {
                // If the file was deleted, we report an error (as the user likely needs to update
                // the LINT directive).
                return self.addDiagnostic(changed_line orelse then_change_line, .{
                    .file_deleted = .{
                        .file_id = file_id,
                        .then_change_line = then_change_line,
                    },
                });
            },
            .renamed_to => |to_file_id| {
                current_file_id = to_file_id orelse {
                    return self.addDiagnostic(changed_line orelse return, .{
                        .file_renamed_but_not_modified = .{
                            .file_id = file_id,
                            .then_change_line = then_change_line,
                        },
                    });
                };
                continue :sw self.analyzer.files[current_file_id].change.status;
            },

            .modified_lines, .modified_ranges => {}, // Keep going.
        }

        const label_name = given_label orelse return; // No label? Check succeeds.
        const labels = file.labelsIfDone() orelse {
            // If labels are not available yet, we report a diagnostic we'll ignore when time comes
            // to report them.
            return self.addDiagnostic(then_change_line, .{
                .label_status_not_yet_available = .{
                    .file_id = file_id,
                    .label = try self.allocator.allocator().dupe(u8, label_name),
                    .changed_line = changed_line,
                },
            });
        };
        const label = labels.get(label_name) orelse return self.addDiagnostic(then_change_line, .{
            .label_does_not_exist = try self.allocator.allocator().dupe(u8, label_name),
        });

        switch (label.status) {
            .changed => {
                // Success!
            },
            .unchanged => {
                return self.addDiagnostic(changed_line orelse return, .{
                    .label_not_modified = .{
                        .label = try self.allocator.allocator().dupe(u8, label_name),
                        .then_change_line = then_change_line,
                    },
                });
            },
        }
    }

    /// Appends a diagnostic to the currently processed file's diagnostics.
    fn addDiagnostic(
        self: *Worker,
        line: u32,
        data: Diagnostic.Details,
    ) error{OutOfMemory}!void {
        _ = try self.file.diagnostics.append(self.allocator.allocator(), .{
            .line = line,
            .details = data,
        });
    }

    /// Returns the file corresponding to the given file.
    fn resolvePath(self: *Worker, raw_path: []const u8, line: u32) error{OutOfMemory}!?[3][]const u8 {
        std.debug.assert(raw_path.len > 0);

        if (raw_path[0] == '/') {
            // We allow paths that start with `//` and interpret them the same as `/`.
            const absolute_path =
                if (raw_path.len > 1 and raw_path[1] == '/') raw_path[2..] else raw_path[1..];

            if (absolute_path.len == 0) {
                try self.addDiagnostic(line, .{
                    .invalid_path = try self.allocator.allocator().dupe(u8, raw_path),
                });
                return null;
            }
            return [3][]const u8{ absolute_path, "", "" };
        }

        // Handle "./".
        var resolved = raw_path;

        if (resolved.len > 2 and resolved[0] == '.' and resolved[1] == '/') {
            // Allow paths to start with "./".
            resolved = resolved[2..];
        }

        // Handle ancestors.
        var current_dir = self.file_dir;

        while (std.mem.startsWith(u8, resolved, "../")) {
            const parent_dir_end = std.mem.lastIndexOfScalar(u8, current_dir, '/') orelse {
                try self.addDiagnostic(line, .{
                    .path_escapes_root = try self.allocator.allocator().dupe(u8, raw_path),
                });
                return null;
            };
            resolved = resolved[3..];
            current_dir = current_dir[0..parent_dir_end];
        }

        if (resolved.len == 0) {
            try self.addDiagnostic(line, .{
                .invalid_path = try self.allocator.allocator().dupe(u8, raw_path),
            });
            return null;
        }

        // Concatenate the current directory and the resolved path.
        if (current_dir.len == 0) {
            return [3][]const u8{ resolved, "", "" };
        }

        // The caller doesn't have to deallocate the returned slice as we use a bump allocator.
        return [3][]const u8{ current_dir, "/", resolved };
    }
};

// -------------------------------------------------------------------------------------------------
// MARK: ConcatStringContext

/// A `std.hash_map` context which allows looking up a string using the concatenation of three
/// substrings.
const ConcatStringContext = struct {
    const Hasher = std.hash.Wyhash;

    // Make sure that we have the same hasher as the default hash map.
    test Hasher {
        try std.testing.expectEqual(std.hash_map.hashString("test"), Hasher.hash(0, "test"));
    }

    pub fn eql(_: @This(), a: [3][]const u8, b: []const u8) bool {
        if (b.len != a[0].len + a[1].len + a[2].len) return false;

        var left = b;

        for (a) |part| {
            if (!std.mem.eql(u8, left[0..part.len], part)) {
                return false;
            }
            left = left[part.len..];
        }
        return true;
    }

    pub fn hash(_: @This(), adapted_key: [3][]const u8) u64 {
        var hasher: Hasher = .init(0);
        hasher.update(adapted_key[0]);
        hasher.update(adapted_key[1]);
        hasher.update(adapted_key[2]);
        return hasher.final();
    }
};
