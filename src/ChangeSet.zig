const ChangeSet = @This();

pub const DiffParser = @import("DiffParser.zig");
const std = @import("std");

/// Information about a modified file.
pub const File = struct {
    /// Path of the file (owned by the `ChangeSet` allocator).
    path: []const u8,

    status: union(enum) {
        /// New file (all lines were changed).
        new,
        /// Binary file (some lines were changed, but we don't know which).
        binary,
        /// The file was deleted (referencing it is invalid).
        ///
        /// We could simply store no file here, but we use `deleted` instead to provide better
        /// diagnostics.
        deleted,
        /// The file was renamed to the file with the given index in `ChangeSet.files`.
        ///
        /// We could simply store no file here, but we use `renamed_to` instead to provide better
        /// diagnostics.
        ///
        /// If `null`, the file was renamed, but not otherwise changed.
        renamed_to: ?u32,

        /// The file was modified, and modified lines are stored as a dense bit set.
        modified_lines: std.bit_set.DynamicBitSetUnmanaged,
        /// The file was modified, and modified lines are stored as an ordered slice of ranges.
        modified_ranges: []const [2]u32,
    },

    pub fn deinit(self: *File, allocator: std.mem.Allocator) void {
        allocator.free(self.path);

        switch (self.status) {
            .new, .binary, .deleted, .renamed_to => {},
            .modified_lines => |*bit_set| {
                bit_set.deinit(allocator);
                self.status = .new;
            },
            .modified_ranges => |ranges| {
                allocator.free(ranges);
                self.status = .new;
            },
        }
    }

    /// Returns the number of the first line in `[start_line + 1, end_line)` which was modified,
    /// or null if no line was modified in this range.
    pub fn firstModifiedLineIn(self: *const File, start_line: u32, end_line: u32) ?u32 {
        return switch (self.status) {
            .new => start_line + 1,
            .binary, .deleted, .renamed_to => null,
            .modified_lines => |modified_lines| for (start_line + 1..end_line) |line| {
                if (line < modified_lines.bit_length and modified_lines.isSet(line)) {
                    break @intCast(line);
                }
            } else null,
            .modified_ranges => |modified_ranges| blk: {
                const found = std.sort.binarySearch([2]u32, modified_ranges, [2]u32{ start_line, end_line }, struct {
                    fn compare(context: [2]u32, range: [2]u32) std.math.Order {
                        if (context[1] <= range[0]) return .lt;
                        if (context[0] >= range[1]) return .gt;
                        return .eq;
                    }
                }.compare);

                break :blk if (found) |index| modified_ranges[index][0] else null;
            },
        };
    }
};

allocator: std.mem.Allocator,
files: std.ArrayList(File) = .empty,
/// Maps file paths to indices in `files`.
file_indices: std.StringHashMapUnmanaged(u32) = .empty,

/// Returns a new empty `ChangeSet` which uses the given allocator for all allocations.
pub fn init(allocator: std.mem.Allocator) ChangeSet {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *ChangeSet) void {
    for (self.files.items) |*file| {
        file.deinit(self.allocator);
    }
    self.files.deinit(self.allocator);
    self.file_indices.deinit(self.allocator);
}

// -------------------------------------------------------------------------------------------------
// MARK: Parsing

/// Parses a diff from `diff_reader`, adding new files to the change set.
///
/// `scratch` is used for temporary allocations, and may be cleared after the function returns.
pub fn parseDiff(
    self: *ChangeSet,
    scratch: std.mem.Allocator,
    diff_reader: *std.Io.Reader,
) (DiffParser.Error || error{ReadFailed})!void {
    var parser: DiffParser = .init(self, scratch);
    defer parser.deinit();

    try parser.parseUntilEnd(diff_reader);
    try parser.finish();
}

/// Adds a new file to the change set, assuming that its data is owned by `self.allocator`.
///
/// Takes ownership of `file` on success, but not on error.
pub fn addFile(self: *ChangeSet, file: File) error{ DuplicateFile, OutOfMemory }!*File {
    // Make sure we can append the file before anything.
    try self.files.ensureUnusedCapacity(self.allocator, 1);

    // Make sure that we don't have this file already.
    const file_index: u32 = @intCast(self.files.items.len);
    const entry = try self.file_indices.getOrPutValue(self.allocator, file.path, file_index);
    if (entry.value_ptr.* != file_index) {
        return error.DuplicateFile;
    }
    errdefer self.file_indices.removeByPtr(entry.key_ptr);

    // Create and append the file.
    self.files.appendAssumeCapacity(file);

    return &self.files.items[self.files.items.len - 1];
}
