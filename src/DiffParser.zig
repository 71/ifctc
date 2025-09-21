//! Sans-IO parser for unified diffs.
const DiffParser = @This();

test {
    _ = @import("DiffParser_test.zig");
}

const ChangeSet = @import("ChangeSet.zig");
const std = @import("std");
const strings = @import("strings.zig");

pub const Error = error{ InvalidDiff, OutOfMemory };
pub const File = ChangeSet.File;

const max_path_bytes: u32 = std.fs.max_path_bytes;
const prefix_bytes: u32 = 4;
const skip_line: u32 = std.math.maxInt(u32);

change_set: *ChangeSet,
scratch: std.mem.Allocator,
line_in_diff: u32 = 1,
line_buffer: [max_path_bytes + prefix_bytes]u8 = undefined,
line_len: u32 = 0,

state: union(enum) {
    /// Status of the file being parsed, if any.
    const Status = enum { new, deleted, renamed };

    /// Expecting a `diff --git ...` header.
    expect_header,
    /// Expecting `rename from` and `rename to` lines (or other extended headers).
    expect_rename: struct {
        seen: ?struct { []u8, ExtendedHeaderType },
        must_add_up_to: usize,
    },
    /// Expecting a `--- a/path` line (or extended headers).
    expect_old_file_path: struct {
        /// Owned file path.
        file_path: []u8,
        status: ?Status,
    },
    /// Expecting a `+++ b/path` line.
    expect_new_file_path: struct {
        /// Owned file path.
        file_path: []u8,
        status: ?Status,
    },
    /// Expecting a hunk header `@@ -1,3 +1,3 @@` or a new file header `diff --git ...`.
    expect_hunk_header: struct {
        /// Owned file path.
        file_path: []u8,
        expect: union(enum) { new, deleted, modified_after_line: u32 },
    },
    /// Expecting hunk lines (starting with ' ', '+', or '-').
    in_hunk: struct {
        /// Owned file path.
        file_path: []u8,
        current_line: u32,
        remaining_added_lines: u32,
        remaining_removed_lines: u32,
        last_line_was_removal: bool,
    },
    /// Expecting addition lines (starting with '+') in a new file.
    in_new_file_hunk: struct {
        /// Owned file path.
        file_path: []u8,
        remaining_added_lines: u32,
    },
    /// Expecting removal lines (starting with '-') in a deleted file.
    in_deleted_file_hunk: struct {
        /// Owned file path.
        file_path: []u8,
        remaining_removed_lines: u32,
    },
} = .expect_header,

/// Modified ranges of lines in current file.
///
/// In theory we should store this in `state`, but we don't to reuse allocations.
modified_ranges: std.ArrayList([2]u32) = .empty,

/// Function used to print error information when returning `InvalidDiff`.
on_error: *const fn ([]const []const u8) void = &logError,

/// Returns a new `DiffParser` which adds new files to `change_set` and uses `scratch` for
/// temporary allocations.
pub fn init(change_set: *ChangeSet, scratch: std.mem.Allocator) DiffParser {
    return .{
        .change_set = change_set,
        .scratch = scratch,
    };
}

pub fn deinit(self: *DiffParser) void {
    self.modified_ranges.deinit(self.scratch);

    switch (self.state) {
        .expect_header => {},
        .expect_rename => |state| {
            if (state.seen) |seen| self.change_set.allocator.free(seen[0]);
        },
        inline .expect_old_file_path, .expect_new_file_path, .expect_hunk_header, .in_hunk, .in_new_file_hunk, .in_deleted_file_hunk => |state| {
            self.change_set.allocator.free(state.file_path);
        },
    }
}

/// Tells the parser that no more input will be provided, ensuring that the current state is
/// valid.
pub fn finish(self: *DiffParser) Error!void {
    if (self.line_len != 0) {
        var buf: []const u8 = "\n";
        if (try self.parseSome(&buf)) |file| try self.addFile(file);
        std.debug.assert(buf.len == 0);
    }
    switch (self.state) {
        .expect_header => {},
        .expect_hunk_header => |state| {
            if (try self.finishFile(state.file_path)) |file| try self.addFile(file);
            std.debug.assert(self.state == .expect_header);
        },
        .in_hunk, .in_new_file_hunk, .in_deleted_file_hunk => {
            return self.invalidError("expected more hunk lines");
        },
        .expect_rename => return self.invalidError("expected `rename from` or `rename to` lines"),
        .expect_old_file_path => |state| {
            try self.addFile(.{
                .path = state.file_path,
                .status = switch (state.status orelse .renamed) {
                    .new, .renamed => .new,
                    .deleted => .deleted,
                },
            });

            self.state = .expect_header;
        },
        .expect_new_file_path => return self.invalidError("expected `+++ <filename>`"),
    }
}

/// Calls `parse()` repeatedly until the end of the stream is reached. `finish()` must still be
/// called after this function.
pub fn parseUntilEnd(self: *DiffParser, reader: *std.io.Reader) (Error || error{ReadFailed})!void {
    while (true) {
        try self.parse(reader.buffered());
        reader.tossBuffered();

        reader.fillMore() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
    }
}

/// Parses a patch from the given bytes. This function may be called repeatedly to parse a
/// concatenation of chunks of a single patch.
pub fn parse(self: *DiffParser, bytes: []const u8) Error!void {
    var left = bytes;

    while (left.len > 0) {
        const len_before = left.len;
        if (try self.parseSome(&left)) |file| {
            try self.addFile(file);
        }
        std.debug.assert(len_before > left.len);
    }
}

/// Parses as much as possible from `bytes`, returning a `File` if a full file has been parsed,
/// and `null` otherwise.
fn parseSome(self: *DiffParser, bytes: *[]const u8) Error!?File {
    std.debug.assert(bytes.len > 0);

    // The diff format is very line-oriented, where the prefix of each line tells us what that
    // line is, so the parser is focused on reading one line at a time. However, most lines are
    // unnecessary to us, so we skip over them instead of reading them fully.

    const line_end = std.mem.indexOfScalar(u8, bytes.*, '\n');

    defer if (line_end) |end| {
        self.line_in_diff += 1;
        self.line_len = 0;
        bytes.* = bytes.*[end + 1 ..];
    } else {
        // Use `bytes[bytes.len..]` instead of `""` when the whole buffer is consumed to make it
        // possible to compute the number of read bytes using `new_bytes.ptr - old_bytes.ptr`.
        bytes.* = bytes.*[bytes.len..];
    };

    if (self.line_len == skip_line) {
        // We were skipping a line, keep going.
        return null;
    }
    if (line_end) |end| {
        var line = bytes.*[0..end];
        return try self.parseLine(&line, true);
    }
    var line = bytes.*;
    return try self.parseLine(&line, false);
}

/// Parses a single line from the current state.
///
/// See https://git-scm.com/docs/git-diff#generate_patch_text_with_p.
fn parseLine(self: *DiffParser, line: *[]const u8, is_full_line: bool) Error!?File {
    const first_char =
        if (self.line_len > 0) self.line_buffer[0] else if (line.len > 0) line.*[0] else null;
    var result: ?File = null;

    sw: switch (self.state) {
        .expect_header => {
            // Expect `diff --git a/path b/path`.
            const expected_prefix = "diff --git ";
            const prefix =
                self.readLineAtLeast(expected_prefix.len, line, is_full_line) orelse return result;

            if (!std.mem.startsWith(u8, prefix, expected_prefix)) {
                // We could be finishing a file with no terminating newline.
                @branchHint(.unlikely);

                const no_newline_message = "\\ No newline at end of file";

                if (prefix.len <= no_newline_message.len and
                    std.mem.startsWith(u8, no_newline_message, prefix))
                {
                    self.skipLine();
                    return result;
                }

                return self.invalidErrorConcat(
                    &[_][]const u8{ "unknown diff header format: ", prefix },
                );
            }

            const full_line =
                try self.readLineUntilEnd(line, is_full_line) orelse return result;
            const paths = full_line[expected_prefix.len..];

            // We expect two paths separated by a space, so at least "a/a b/a" (7 characters).
            if (paths.len < 7) {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "invalid diff --git line: ", full_line },
                );
            }

            // Extract the diff path.
            const path = diffLinePath(paths) catch |err| switch (err) {
                error.InvalidDiff => return self.invalidErrorConcat(
                    &[_][]const u8{ "invalid diff --git line: ", full_line },
                ),
            } orelse {
                // We have a seemingly valid line, but two different paths; this is likely a rename.
                self.state = .{
                    .expect_rename = .{
                        .seen = null,
                        // We expect `from.len + to.len` to add up to `paths.len - 5` (both paths,
                        // minus `a/`, `b/`, and ` `).
                        .must_add_up_to = paths.len - 5,
                    },
                };
                self.skipLine();
                return result;
            };
            const owned_path = try self.change_set.allocator.dupe(u8, path);

            self.state = .{
                .expect_old_file_path = .{ .file_path = owned_path, .status = null },
            };
            self.skipLine();
        },
        .expect_rename => |*state| {
            // Skip over extended header lines.
            const full_line =
                try self.readLineUntilEnd(line, is_full_line) orelse return null;
            const header_type, const path_offset = parseExtendedHeaderLine(full_line) orelse {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "expected rename extended header line, got: ", full_line },
                );
            };
            switch (header_type) {
                .rename_from, .rename_to => {
                    // Keep going.
                },
                else => {
                    // Skip extended header line.
                    self.skipLine();
                    return null;
                },
            }

            // Process `rename from` and `rename to` lines.
            const path = full_line[path_offset..];
            const seen_path, const seen_type = state.seen orelse {
                state.seen = .{ try self.change_set.allocator.dupe(u8, path), header_type };

                self.skipLine();
                return null;
            };
            if (seen_type == header_type) {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "duplicate `", full_line[0 .. path_offset - 1], "` header" },
                );
            }
            const from_path, const to_path = switch (seen_type) {
                .rename_from => .{ seen_path, try self.change_set.allocator.dupe(u8, path) },
                .rename_to => .{ try self.change_set.allocator.dupe(u8, path), seen_path },
                else => unreachable,
            };
            const combined_len = from_path.len + to_path.len;

            if (combined_len != state.must_add_up_to) {
                const from_len: IntString = .init(from_path.len);
                const to_len: IntString = .init(to_path.len);
                const must_add_up_to: IntString = .init(state.must_add_up_to);

                return self.invalidErrorConcat(&[_][]const u8{
                    "`rename from` and `rename to` lengths do not add up to paths in `diff --git` header: ",
                    from_len.get(),
                    " + ",
                    to_len.get(),
                    " != ",
                    must_add_up_to.get(),
                });
            }

            // Keep going, focusing on the new file path.
            self.state = .{
                .expect_old_file_path = .{
                    .file_path = to_path,
                    .status = .renamed,
                },
            };
            self.skipLine();

            // Create the rename file entry.
            return .{
                .path = from_path,
                .status = .{
                    // We know that the next file we parse is the renamed file, so we can
                    // directly refer to its index here.
                    .renamed_to = @intCast(self.change_set.files.items.len + 1),
                },
            };
        },
        .expect_old_file_path => |*state| {
            // Skip over extended header lines.
            const prefix =
                self.readLineAtLeast(longest_extended_header_line_prefix, line, is_full_line) orelse return null;

            if (parseExtendedHeaderLine(prefix)) |header_type| {
                switch (header_type[0]) {
                    .new_file_mode => if (state.status != null) {
                        return self.invalidError("incompatible or duplicate headers given with `new file mode` header");
                    } else {
                        state.status = .new;
                    },
                    .deleted_file_mode => if (state.status != null) {
                        return self.invalidError("incompatible or duplicate headers given with `deleted file mode` header");
                    } else {
                        state.status = .deleted;
                    },
                    .rename_from, .rename_to => if (state.status == .renamed) {
                        return self.invalidError("duplicate `rename from` or `rename to` header");
                    } else if (state.status != null) {
                        return self.invalidError("incompatible headers given with `rename from` or `rename to` header");
                    } else {
                        state.status = .renamed;
                    },
                    else => {},
                }
                self.skipLine();
                return null;
            }

            // Expect `--- a/path`.
            const full_line =
                try self.readLineUntilEnd(line, is_full_line) orelse return null;

            if (!std.mem.startsWith(u8, full_line, "--- ")) {
                const expected = state.status orelse return self.invalidErrorConcat(
                    &[_][]const u8{ "expected `--- <filename>`, got: ", full_line },
                );

                // For new, deleted, and renamed files, we may not have any hunk, in which case
                // the new/old file paths are optional.
                result = .{
                    .path = state.file_path,
                    .status = switch (expected) {
                        // If a file is the target of a rename, it is new.
                        .new, .renamed => .new,
                        .deleted => .deleted,
                    },
                };

                self.state = .expect_header;

                continue :sw self.state;
            }

            const raw_path = trimTimestamp(full_line[4..]);

            if (std.mem.eql(u8, raw_path, "/dev/null")) {
                // We expect a new file.
                if (state.status != .new) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "diff has `--- /dev/null`, but did not have a `new file` header: ",
                        state.file_path,
                    });
                }
            } else {
                // We expect the path to be `a/path`.
                if (raw_path.len < 3 or !std.ascii.isAlphabetic(raw_path[0]) or raw_path[1] != '/') {
                    return self.invalidErrorConcat(&[_][]const u8{ "invalid file path: ", raw_path });
                }
                const path = raw_path[2..];

                if (state.status != .renamed and !std.mem.eql(u8, path, state.file_path)) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "file path in `--- <filename>` does not match `diff --git` header: ",
                        path,
                        " != ",
                        state.file_path,
                    });
                }

                if (state.status == .new) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "diff had `new file` header, but is not `--- /dev/null`: ",
                        state.file_path,
                    });
                }
            }

            self.state = .{
                .expect_new_file_path = .{ .file_path = state.file_path, .status = state.status },
            };
        },
        .expect_new_file_path => |state| {
            // Expect `+++ b/path`.
            const full_line =
                try self.readLineUntilEnd(line, is_full_line) orelse return null;
            if (!std.mem.startsWith(u8, full_line, "+++ ")) {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "expected `+++ <filename>`, got: ", full_line },
                );
            }
            const raw_path = trimTimestamp(full_line[4..]);

            if (std.mem.eql(u8, raw_path, "/dev/null")) {
                // We expect a deleted file.
                if (state.status != .deleted) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "diff has `+++ /dev/null`, but did not have an `old file` header: ",
                        state.file_path,
                    });
                }
            } else {
                // We expect the path to be `b/path`.
                if (raw_path.len < 3 or !std.ascii.isAlphabetic(raw_path[0]) or raw_path[1] != '/') {
                    // We don't update the state, so we don't free `old_path`.
                    return self.invalidErrorConcat(&[_][]const u8{ "invalid file path: ", raw_path });
                }
                const path = raw_path[2..];

                if (!std.mem.eql(u8, path, state.file_path)) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "file path in `+++ <filename>` does not match `diff --git` header: ",
                        path,
                        " != ",
                        state.file_path,
                    });
                }

                if (state.status == .deleted) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "diff had `old file` header, but is not `+++ /dev/null`: ",
                        state.file_path,
                    });
                }
            }

            self.state = .{
                .expect_hunk_header = .{
                    .file_path = state.file_path,
                    .expect = switch (state.status orelse .renamed) {
                        .new => .new,
                        .deleted => .deleted,
                        .renamed => .{ .modified_after_line = 0 },
                    },
                },
            };
        },
        .expect_hunk_header => |*state| {
            // Rather than a hunk, we might be reading a new file header.
            const is_new_header = first_char == 'd' or first_char == '\\';
            if (is_new_header) {
                result = try self.finishFile(state.file_path);
                std.debug.assert(self.state == .expect_header);
                continue :sw self.state;
            }

            const full_line =
                try self.readLineUntilEnd(line, is_full_line) orelse return null;

            // We could also be reading a binary file change information.
            if (std.mem.startsWith(u8, full_line, "Binary ")) {
                defer self.state = .expect_header;

                return .{
                    .path = state.file_path,
                    .status = .binary,
                };
            }

            // Expect `@@ -1,3 +1,3 @@ ...`.
            const hunk_header = parseHunkHeader(full_line) catch {
                return self.invalidErrorConcat(&[_][]const u8{ "invalid hunk header: ", full_line });
            };

            if (hunk_header.old_start == 0 or hunk_header.old_lines == 0) {
                if (hunk_header.old_start != 0 or hunk_header.old_lines != 0) {
                    // Both must be 0.
                    return self.invalidErrorConcat(&[_][]const u8{ "invalid hunk header: ", full_line });
                }
                if (state.expect != .new) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "hunk header with -0,0 must be for a new file: ",
                        state.file_path,
                    });
                }

                // This is a file addition.
                self.state = .{
                    .in_new_file_hunk = .{
                        .file_path = state.file_path,
                        .remaining_added_lines = hunk_header.new_lines,
                    },
                };
                return null;
            }

            // Checking this _after_ the above `if` lets us accept empty new files.
            if (hunk_header.new_start == 0 or hunk_header.new_lines == 0) {
                if (hunk_header.new_start != 0 or hunk_header.new_lines != 0) {
                    return self.invalidErrorConcat(&[_][]const u8{ "invalid hunk header: ", full_line });
                }
                if (state.expect != .deleted) {
                    return self.invalidErrorConcat(&[_][]const u8{
                        "hunk header with +0,0 must be for a deleted file: ",
                        state.file_path,
                    });
                }

                // This is a file deletion.
                self.state = .{
                    .in_deleted_file_hunk = .{
                        .file_path = state.file_path,
                        .remaining_removed_lines = hunk_header.old_lines,
                    },
                };
                return null;
            }

            const after_line = switch (state.expect) {
                .modified_after_line => |l| l,
                .new => return self.invalidErrorConcat(&[_][]const u8{
                    "only a hunk header with -0,0 may be used for a new file: ",
                    full_line,
                }),
                .deleted => return self.invalidErrorConcat(&[_][]const u8{
                    "only a hunk header with +0,0 may be used for a deleted file: ",
                    full_line,
                }),
            };
            if (hunk_header.new_start < after_line) {
                return self.invalidError("hunks must be ordered");
            }

            self.state = .{
                .in_hunk = .{
                    .file_path = state.file_path,
                    .current_line = hunk_header.new_start,
                    .remaining_added_lines = hunk_header.new_lines,
                    .remaining_removed_lines = hunk_header.old_lines,
                    .last_line_was_removal = false,
                },
            };
        },
        .in_new_file_hunk => |*state| {
            if (first_char != '+') {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "expected addition line in ", state.file_path },
                );
            }
            if (state.remaining_added_lines == 0) {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "too many additions in ", state.file_path },
                );
            }
            self.skipLine();
            state.remaining_added_lines -= 1;

            if (state.remaining_added_lines == 0) {
                // That's the end of the current hunk.
                defer self.state = .expect_header;
                return .{ .path = state.file_path, .status = .new };
            }
        },
        .in_deleted_file_hunk => |*state| {
            if (first_char != '-') {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "expected deletion line in ", state.file_path },
                );
            }
            if (state.remaining_removed_lines == 0) {
                return self.invalidErrorConcat(
                    &[_][]const u8{ "too many deletions in ", state.file_path },
                );
            }
            self.skipLine();
            state.remaining_removed_lines -= 1;

            if (state.remaining_removed_lines == 0) {
                // That's the end of the current hunk.
                defer self.state = .expect_header;
                return .{ .path = state.file_path, .status = .deleted };
            }
        },
        .in_hunk => |*state| {
            // https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html#Detailed-Description-of-Unified-Format
            //
            // We _should_ start with one of "-", "+" or " ", but the diff might have been
            // post-processed in a way that would remove trailing whitespace, in which case
            // " " would become empty.
            switch (first_char orelse ' ') {
                ' ' => {
                    // Context line.
                    if (state.remaining_added_lines == 0) {
                        return self.invalidErrorConcat(
                            &[_][]const u8{ "too many additions in ", state.file_path },
                        );
                    }
                    if (state.remaining_removed_lines == 0) {
                        return self.invalidErrorConcat(
                            &[_][]const u8{ "too many removals in ", state.file_path },
                        );
                    }
                    state.remaining_added_lines -= 1;
                    state.remaining_removed_lines -= 1;
                    state.current_line += 1;
                },
                '-', '+' => {
                    // Added / removed line.
                    if (self.modified_ranges.items.len == 0 or self.modified_ranges.getLast()[1] != state.current_line) {
                        // Start a new range.
                        try self.modified_ranges.append(self.scratch, .{ state.current_line, state.current_line });
                    } else if (first_char == '+' and !state.last_line_was_removal) {
                        // Extend the last range (if this is an addition).
                        self.modified_ranges.items[self.modified_ranges.items.len - 1][1] += 1;
                    }
                    state.last_line_was_removal = first_char == '-';

                    if (first_char == '+') {
                        if (state.remaining_added_lines == 0) {
                            return self.invalidErrorConcat(
                                &[_][]const u8{ "too many additions in ", state.file_path },
                            );
                        }
                        state.current_line += 1;
                        state.remaining_added_lines -= 1;
                    } else {
                        if (state.remaining_removed_lines == 0) {
                            return self.invalidErrorConcat(
                                &[_][]const u8{ "too many removals in ", state.file_path },
                            );
                        }
                        state.remaining_removed_lines -= 1;
                    }
                },
                else => return self.invalidErrorConcat(
                    &[_][]const u8{ "expected context, addition or removal line in ", state.file_path },
                ),
            }

            if (state.remaining_added_lines == 0 and state.remaining_removed_lines == 0) {
                // That's the end of the current hunk.
                self.state = .{
                    .expect_hunk_header = .{
                        .file_path = state.file_path,
                        .expect = .{ .modified_after_line = state.current_line },
                    },
                };
            }

            self.skipLine();
        },
    }

    return result;
}

/// Finishes the current file, updating its `changes` and the current state.
fn finishFile(self: *DiffParser, owned_file_path: []const u8) Error!?File {
    defer self.state = .expect_header;
    errdefer self.change_set.allocator.free(owned_file_path);

    const last_range = self.modified_ranges.getLastOrNull() orelse {
        // No changes in this file. This happens for instance if the file was only renamed or
        // its mode changed. In this case we don't consider that the file was modified, and just
        // skip it.
        self.change_set.allocator.free(owned_file_path);

        const files = self.change_set.files.items;

        if (files.len > 0 and
            files[files.len - 1].status == .renamed_to and
            files[files.len - 1].status.renamed_to == @as(u32, @intCast(files.len)))
        {
            // The file was renamed; clean up the renamed file.
            files[files.len - 1].status = .{ .renamed_to = null };
        }
        return null;
    };
    const max_modified_line = last_range[1];
    defer self.modified_ranges.clearRetainingCapacity();

    const allocator = self.change_set.allocator;
    const store_in_bitset = max_modified_line < 128 or self.modified_ranges.items.len > (max_modified_line / 8);

    if (store_in_bitset) {
        var modified_lines = try std.DynamicBitSetUnmanaged.initEmpty(allocator, max_modified_line + 1);
        errdefer modified_lines.deinit(allocator);

        for (self.modified_ranges.items) |range| {
            for (range[0]..range[1] + 1) |line| {
                modified_lines.set(line);
            }
        }

        return .{
            .path = owned_file_path,
            .status = .{ .modified_lines = modified_lines },
        };
    } else {
        return .{
            .path = owned_file_path,
            .status = .{
                .modified_ranges = try allocator.dupe([2]u32, self.modified_ranges.items),
            },
        };
    }
}

/// Wrapper around `ChangeSet.addFile()` which transforms `DuplicateFile` errors into `InvalidDiff`.
fn addFile(self: *DiffParser, file: File) Error!void {
    errdefer {
        var f = file;
        f.deinit(self.change_set.allocator);
    }

    _ = self.change_set.addFile(file) catch |err| switch (err) {
        error.DuplicateFile => {
            return self.invalidErrorConcat(&[_][]const u8{ "file appears multiple times in diff: ", file.path });
        },
        else => |e| return e,
    };
}

/// Tells the parsing loop to skip the current line.
fn skipLine(self: *DiffParser) void {
    self.line_len = skip_line;
}

/// Reads the current line (continuing with `line`), returning it if it is fully read.
fn readLineUntilEnd(
    self: *DiffParser,
    line: *[]const u8,
    is_full_line: bool,
) error{InvalidDiff}!?[]const u8 {
    if (is_full_line and self.line_len == 0) {
        // We have the full line in `line`.
        return line.*;
    }

    if (self.line_len + line.len > self.line_buffer.len) {
        return self.invalidError("line too long");
    }

    @memcpy(self.line_buffer[self.line_len .. self.line_len + line.len], line.*);
    self.line_len += @intCast(line.len);
    line.* = line.*[line.len..];

    if (!is_full_line) {
        return null;
    }
    // We have a full line after copying from `line`.
    return self.line_buffer[0..self.line_len];
}

/// Reads the current line (continuing with `line`) until at least `count` bytes have been read.
///
/// Returns `null` if `count` bytes have not yet been read **and** `is_full_line` is false.
/// That is, this function may return a slice smaller than `count` if the end of the line is
/// reached.
fn readLineAtLeast(
    self: *DiffParser,
    count: usize,
    line: *[]const u8,
    is_full_line: bool,
) ?[]const u8 {
    if (self.line_len == 0 and (is_full_line or line.len >= count)) {
        // We have what we need in `line`.
        return line.*;
    }
    if (self.line_len >= count) {
        // We have what we need in `line_buffer`.
        return self.line_buffer[0..count];
    }

    if (self.line_len + line.len >= count) {
        // We have what we need after copying from `line`.
        @memcpy(self.line_buffer[self.line_len..count], line.*[0 .. count - self.line_len]);
        line.* = line.*[count - self.line_len ..];
        self.line_len = @intCast(count);
        return self.line_buffer[0..count];
    }

    @memcpy(self.line_buffer[self.line_len .. self.line_len + line.len], line.*);
    self.line_len += @intCast(line.len);
    line.* = line.*[line.len..];

    if (!is_full_line) {
        return null;
    }
    // We have a full line after copying from `line`.
    return self.line_buffer[0..self.line_len];
}

/// Reports an error using `on_error`.
///
/// `inline` to produce more concise stack traces.
inline fn invalidError(self: *const DiffParser, message: []const u8) error{InvalidDiff} {
    return self.invalidErrorConcat(&[1][]const u8{message});
}

/// Reports an error using `on_error`. `message` must be a slice of string slices which will be
/// concatenated.
///
/// `message` is `anytype` instead of `[]const []const u8` because we need to know its size at
/// comptime to prepend it with the line number.
///
/// `inline` to produce more concise stack traces.
inline fn invalidErrorConcat(self: *const DiffParser, message: anytype) error{InvalidDiff} {
    const fmt = "line {}: ";
    const max_prefix_len = std.fmt.comptimePrint(fmt, .{std.math.maxInt(u32)}).len;
    var prefix_buf: [max_prefix_len]u8 = undefined;
    const prefix = std.fmt.bufPrint(&prefix_buf, fmt, .{self.line_in_diff}) catch unreachable;
    (self.on_error)([1][]const u8{prefix} ++ message);
    return error.InvalidDiff;
}

/// Default error function which logs the error with `std.log.err()`.
fn logError(message: []const []const u8) void {
    const Concat = struct {
        slices: []const []const u8,

        pub fn format(self: *const @This(), writer: *std.io.Writer) std.io.Writer.Error!void {
            for (self.slices) |slice| {
                try writer.writeAll(slice);
            }
        }
    };

    std.log.err("{f}", .{Concat{ .slices = message }});
}

/// Formats an `usize` to a buffer.
const IntString = struct {
    buffer: [std.fmt.comptimePrint("{d}", .{std.math.maxInt(usize)}).len]u8,
    len: u8,

    pub fn init(value: usize) IntString {
        var self: IntString = .{ .buffer = undefined, .len = undefined };
        const slice = std.fmt.bufPrint(&self.buffer, "{d}", .{value}) catch unreachable;
        self.len = @intCast(slice.len);
        return self;
    }

    pub fn get(self: *const @This()) []const u8 {
        return self.buffer[0..self.len];
    }
};

// -------------------------------------------------------------------------------------------------
// MARK: Parse helpers

/// Extracts, given a line expected to match `a/a b/a`, the path `a`.
///
/// Returns `null` if the two paths are not equal, and `error.InvalidDiff` is the line is otherwise
/// invalid.
fn diffLinePath(paths: []const u8) error{InvalidDiff}!?[]const u8 {
    std.debug.assert(paths.len >= 7); // Verified by caller.

    if (!std.ascii.isAlphabetic(paths[0]) or paths[1] != '/') {
        // Path cannot be valid.
        return error.InvalidDiff;
    }

    // Test the happy path first: both paths are equal after removing the `a/` and `b/` prefixes.
    const middle = paths.len / 2;

    if (paths[middle] == ' ' and
        std.ascii.isAlphabetic(paths[middle + 1]) and
        paths[middle + 2] == '/' and
        std.mem.eql(u8, paths[2..middle], paths[middle + 3 ..]))
    {
        @branchHint(.likely);

        return paths[2..middle];
    }

    // Since there is no escaping, we could get in pretty bad cases where a file
    // is (validly) renamed from "a/a b/a" to "a/a b/a" or something like this,
    // which we must accept.
    //
    // We therefore try to find the middle space, followed by a valid `b/`
    // prefix, and consider that a rename if we find it.
    const fns = struct {
        fn is_match(bytes: [1]u8) bool {
            return std.ascii.isAlphabetic(bytes[0]);
        }

        fn find_linear(haystack: []const u8, offset: usize) ?usize {
            var pos = offset;

            while (true) {
                const space_index = std.mem.indexOfScalarPos(
                    u8,
                    haystack[0 .. haystack.len - 2],
                    pos,
                    ' ',
                ) orelse return null;

                if (std.ascii.isAlphabetic(haystack[space_index + 1]) and haystack[space_index + 2] == '/') {
                    return space_index;
                }

                pos = space_index + 1;
            }
        }
    };
    // Only search for a subslice of the paths:
    // - Skip `a/?` (3 bytes) at the start, to make sure that `a/` is followed by _something_.
    // - Skip 1 byte at the end to avoid matching a trailing ` b/` followed by nothing.
    const haystack = paths[3 .. paths.len - 1];

    _ = strings.indexOfSurrounded(
        haystack,
        ' ',
        '/',
        3,
        fns.is_match,
        fns.find_linear,
    ) orelse return error.InvalidDiff;

    return null; // Likely a rename, gotta keep scanning.
}

test diffLinePath {
    // Make sure to test with paths of odd and even lengths since we have a division by 2.
    try std.testing.expectEqualStrings("1", (try diffLinePath("a/1 b/1")).?);
    try std.testing.expectEqualStrings("ab", (try diffLinePath("a/ab b/ab")).?);
    try std.testing.expectEqualStrings("a/b", (try diffLinePath("a/a/b a/a/b")).?);

    // A rename:
    try std.testing.expectEqual(null, try diffLinePath("a/foo b/bar"));
    try std.testing.expectEqual(null, try diffLinePath("a/foo b/fooo"));
    try std.testing.expectEqual(null, try diffLinePath("a/fooo b/foo"));

    // Invalid lines:
    try std.testing.expectError(error.InvalidDiff, diffLinePath("invalid"));
    try std.testing.expectError(error.InvalidDiff, diffLinePath("a/a_b/a"));
    try std.testing.expectError(error.InvalidDiff, diffLinePath("invalid invalid"));
    try std.testing.expectError(error.InvalidDiff, diffLinePath("/invalid /invalid"));
    try std.testing.expectError(error.InvalidDiff, diffLinePath("a/ b/bb"));
    try std.testing.expectError(error.InvalidDiff, diffLinePath("a/aa b/"));
}

/// Returns `line` with a trailing timestamp removed, if any.
fn trimTimestamp(line: []const u8) []const u8 {
    // https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html:
    //
    // > The timestamp looks like '2002-02-21 23:30:39.942229878 -0800' [...]. The fractional
    // > seconds are omitted on hosts that do not support fractional timestamps.
    var res = line;

    if (res.len < 20) return res;

    if (res[res.len - 5] == '-' or res[res.len - 5] == '+') {
        // Likely the timezone offset.
        if (res[res.len - 6] == ' ' and
            std.ascii.isDigit(res[res.len - 4]) and
            std.ascii.isDigit(res[res.len - 3]) and
            std.ascii.isDigit(res[res.len - 2]) and
            std.ascii.isDigit(res[res.len - 1]))
        {
            // Skip the timezone offset.
            res = res[0 .. res.len - 6];
        } else {
            // The timestamp can't contain '-' or '+', bail out.
            return line;
        }
    }

    if (std.mem.lastIndexOfScalar(u8, res, '.')) |dot_index| {
        // Likely fractional seconds.
        const all_digits = for (res[dot_index + 1 ..]) |c| {
            if (!std.ascii.isDigit(c)) {
                // Even if there are non-digits, this may simply mean that there are no fractional
                // seconds, and the dot was in the middle of the actual path.
                break false;
            }
        } else true;

        if (all_digits) {
            // Skip the fractional seconds.
            res = res[0..dot_index];
        }
    }

    const expected: []const u8 = " 0000-00-00 00:00:00";
    if (res.len < expected.len) return line;

    const actual = res[res.len - expected.len ..];

    inline for (expected, 0..) |c, i| {
        switch (c) {
            '0' => if (!std.ascii.isDigit(actual[i])) return line,
            else => if (actual[i] != c) return line,
        }
    }

    return res[0 .. res.len - expected.len];
}

test trimTimestamp {
    // Example from documentation.
    try std.testing.expectEqualStrings("a/b", trimTimestamp("a/b 2002-02-21 23:30:39.942229878 -0800"));

    // Basic timestamps.
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56"));
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56.789"));
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56.789 +0200"));
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56.789 -0200"));
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56 +0200"));
    try std.testing.expectEqualStrings("foo", trimTimestamp("foo 2025-01-01 12:34:56 -0200"));

    // No timestamp.
    try std.testing.expectEqualStrings("", trimTimestamp(""));
    try std.testing.expectEqualStrings("2025-01-01", trimTimestamp("2025-01-01"));
    try std.testing.expectEqualStrings("2025-01-01 12.34.56", trimTimestamp("2025-01-01 12.34.56"));

    // Timestamp in file name.
    try std.testing.expectEqualStrings("foo2025-01-01 12:34:56", trimTimestamp("foo2025-01-01 12:34:56"));
    try std.testing.expectEqualStrings("foo.bar", trimTimestamp("foo.bar"));
}

/// Parses a hunk header line as per
/// https://git-scm.com/docs/gitattributes#_defining_a_custom_hunk_header.
fn parseHunkHeader(line: []const u8) error{InvalidDiff}!struct {
    old_start: u32,
    old_lines: u32,
    new_start: u32,
    new_lines: u32,
} {
    const min_line_length = "@@ -1 +1 @@".len;
    if (line.len < min_line_length) return error.InvalidDiff;

    if (!std.mem.startsWith(u8, line, "@@ ")) return error.InvalidDiff;

    // Determine start and end of each range.
    const minus_index = try indexOfPos('-', line, 3);
    const plus_index = try indexOfPos('+', line, minus_index + 1);
    const old_space_index = plus_index - 1;
    if (line[old_space_index] != ' ') return error.InvalidDiff;
    const new_space_index = try indexOfPos(' ', line, plus_index + 1);
    if (new_space_index + 2 >= line.len or line[new_space_index + 1] != '@' or line[new_space_index + 2] != '@') return error.InvalidDiff;

    // Parse each range.
    var old_start: u32 = undefined;
    var old_lines: u32 = 1;
    var new_start: u32 = undefined;
    var new_lines: u32 = 1;

    if (std.mem.indexOfScalarPos(u8, line[0..old_space_index], minus_index + 1, ',')) |comma_index| {
        old_start = try parseDecimalU32(line[minus_index + 1 .. comma_index]);
        old_lines = try parseDecimalU32(line[comma_index + 1 .. old_space_index]);
    } else {
        old_start = try parseDecimalU32(line[minus_index + 1 .. old_space_index]);
    }

    if (std.mem.indexOfScalarPos(u8, line[0..new_space_index], plus_index + 1, ',')) |comma_index| {
        new_start = try parseDecimalU32(line[plus_index + 1 .. comma_index]);
        new_lines = try parseDecimalU32(line[comma_index + 1 .. new_space_index]);
    } else {
        new_start = try parseDecimalU32(line[plus_index + 1 .. new_space_index]);
    }

    return .{
        .old_start = old_start,
        .old_lines = old_lines,
        .new_start = new_start,
        .new_lines = new_lines,
    };
}

test parseHunkHeader {
    const Hunk = @TypeOf(try parseHunkHeader(""));

    try std.testing.expectEqual(
        Hunk{ .old_start = 0, .old_lines = 0, .new_start = 0, .new_lines = 0 },
        try parseHunkHeader("@@ -0,0 +0,0 @@"),
    );
    try std.testing.expectEqual(
        Hunk{ .old_start = 1, .old_lines = 1, .new_start = 1, .new_lines = 1 },
        try parseHunkHeader("@@ -1 +1 @@"),
    );
    try std.testing.expectEqual(
        Hunk{ .old_start = 1, .old_lines = 1, .new_start = 1, .new_lines = 1 },
        try parseHunkHeader("@@ -1,1 +1,1 @@"),
    );
    try std.testing.expectEqual(
        Hunk{ .old_start = 1, .old_lines = 3, .new_start = 1, .new_lines = 3 },
        try parseHunkHeader("@@ -1,3 +1,3 @@"),
    );
    try std.testing.expectEqual(
        Hunk{ .old_start = 1, .old_lines = 2, .new_start = 1, .new_lines = 2 },
        try parseHunkHeader("@@ -1,2 +1,2 @@ more text..."),
    );
}

/// Type of an extended header recognized by `isExtendedHeaderLine()`.
const ExtendedHeaderType = enum {
    old_mode,
    new_mode,
    deleted_file_mode,
    new_file_mode,
    copy_from,
    copy_to,
    rename_from,
    rename_to,
    similarity_index,
    dissimilarity_index,
    index,
};
/// Prefixes of extended header lines recognized by `isExtendedHeaderLine()`.
const extended_header_line_prefixes = blk: {
    const enumerants = @typeInfo(ExtendedHeaderType).@"enum".fields;
    var prefixes: [enumerants.len][]const u8 = undefined;
    for (enumerants, 0..) |field, i| {
        var prefix: []const u8 = "";
        for (field.name) |c| {
            prefix = prefix ++ &[_]u8{if (c == '_') ' ' else c};
        }
        prefixes[i] = prefix ++ " ";
    }
    break :blk prefixes;
};
const longest_extended_header_line_prefix = blk: {
    var max: usize = 0;
    for (extended_header_line_prefixes) |prefix| {
        max = @max(max, prefix.len);
    }
    break :blk max;
};

/// Returns whether `line` is an "extended header line" as per
/// https://git-scm.com/docs/git-diff#generate_patch_text_with_p.
fn parseExtendedHeaderLine(line: []const u8) ?struct { ExtendedHeaderType, usize } {
    const PrefixAndType = struct { []const u8, ExtendedHeaderType };
    const prefixes_by_first_char = comptime blk: {
        var prefixes_by_first_char = [1][]const PrefixAndType{&[0]PrefixAndType{}} ** 26;

        for (extended_header_line_prefixes, 0..) |prefix, i| {
            prefixes_by_first_char[prefix[0] - 'a'] =
                prefixes_by_first_char[prefix[0] - 'a'] ++ [1]PrefixAndType{.{ prefix, @enumFromInt(i) }};
        }

        break :blk prefixes_by_first_char;
    };

    if (line.len == 0 or line[0] < 'a' or line[0] > 'z') {
        return null;
    }

    const prefixes_for_first_char = prefixes_by_first_char[line[0] - 'a'];

    for (prefixes_for_first_char) |prefix| {
        if (std.mem.startsWith(u8, line, prefix[0])) {
            return .{ prefix[1], prefix[0].len };
        }
    }

    return null;
}

test parseExtendedHeaderLine {
    try std.testing.expectEqual(.{ ExtendedHeaderType.copy_from, 10 }, parseExtendedHeaderLine("copy from a.txt"));
    try std.testing.expectEqual(.{ ExtendedHeaderType.copy_to, 8 }, parseExtendedHeaderLine("copy to b.txt"));
    try std.testing.expectEqual(.{ ExtendedHeaderType.deleted_file_mode, 18 }, parseExtendedHeaderLine("deleted file mode 100644"));
    try std.testing.expectEqual(null, parseExtendedHeaderLine("boo"));
}

/// Returns the position of the first occurrence of `needle` in `s`, starting from `start`.
inline fn indexOfPos(needle: u8, s: []const u8, start: usize) error{InvalidDiff}!usize {
    return std.mem.indexOfScalarPos(u8, s, start, needle) orelse return error.InvalidDiff;
}

/// Parses an `u32` from the given string in base 10.
///
/// We do not use `std.fmt.parseUnsigned()` as we don't want support for underscores or different
/// bases.
fn parseDecimalU32(s: []const u8) error{InvalidDiff}!u32 {
    if (s.len == 0) return error.InvalidDiff;

    var result: u32 = 0;

    for (s) |c| {
        if (c < '0' or c > '9') return error.InvalidDiff;

        const digit = c - '0';
        const result_times_10, const mul_overflow = @mulWithOverflow(result, 10);
        if (mul_overflow == 1) return error.InvalidDiff;
        const result_plus_digit, const add_overflow = @addWithOverflow(result_times_10, digit);
        if (add_overflow == 1) return error.InvalidDiff;

        result = result_plus_digit;
    }

    return result;
}

test parseDecimalU32 {
    try std.testing.expect(try parseDecimalU32("0") == 0);
    try std.testing.expect(try parseDecimalU32("1") == 1);
    try std.testing.expect(try parseDecimalU32("42") == 42);
    try std.testing.expect(try parseDecimalU32("4294967295") == 4294967295);

    try std.testing.expectError(error.InvalidDiff, parseDecimalU32(""));
    try std.testing.expectError(error.InvalidDiff, parseDecimalU32("a"));
    try std.testing.expectError(error.InvalidDiff, parseDecimalU32("1a"));
    try std.testing.expectError(error.InvalidDiff, parseDecimalU32("4294967296"));
}
