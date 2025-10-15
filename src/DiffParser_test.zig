const ChangeSet = @import("ChangeSet.zig");
const DiffParser = @import("DiffParser.zig");
const std = @import("std");
const test_helpers = @import("test_helpers.zig");

test ChangeSet {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 1, 1 },
                },
            },
        },
    },
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1 +1 @@
        \\-a
        \\+b
    );
}

test "empty diff" {
    try expectDiff(&[_]ChangeSet.File{}, "");
}

test "empty context line" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 2, 2 },
                },
            },
        },
    },
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1 +1,2 @@
        \\
        \\+b
    );
}

test "new file" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "root.zig",
            .status = .new,
        },
    },
        \\diff --git a/root.zig b/root.zig
        \\new file mode 100644
        \\index 0000000..f7553c5
        \\--- /dev/null
        \\+++ b/root.zig
        \\@@ -0,0 +1,2 @@
        \\+pub const Analyzer = @import("Analyzer.zig");
        \\+pub const ChangeSet = @import("ChangeSet.zig");
    );
}

test "empty new file" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .new,
        },
    },
        \\diff --git a/foo b/foo
        \\new file mode 100644
        \\index 0000000..e69de29
    );
}

test "deleted file" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .deleted,
        },
    },
        \\diff --git a/foo b/foo
        \\deleted file mode 100644
        \\index 22dcabc..0000000
        \\--- a/foo
        \\+++ /dev/null
        \\@@ -1 +0,0 @@
        \\-pub const Diagnostic = @This();
    );
}

test "empty deleted file" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .deleted,
        },
    },
        \\diff --git a/foo b/foo
        \\deleted file mode 100644
        \\index e69de29bb2..0000000000
    );
}

test "two files" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "foo",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 1, 1 },
                },
            },
        },
        .{
            .path = "bar",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 1, 1 },
                },
            },
        },
    },
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1 +1 @@
        \\-a
        \\+b
        \\diff --git a/bar b/bar
        \\--- a/bar
        \\+++ b/bar
        \\@@ -1 +1 @@
        \\-a
        \\+b
    );
}

test "no newline at end of file" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = ".gitignore",
            .status = .new,
        },
    },
        \\diff --git a/.gitignore b/.gitignore
        \\new file mode 100644
        \\index 0000000000..b2c08d9bd6
        \\--- /dev/null
        \\+++ b/.gitignore
        \\@@ -0,0 +1,3 @@
        \\+.zig-cache/
        \\+zig-out/
        \\+test.patch
        \\\ No newline at end of file
    );

    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "a",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 1, 1 },
                },
            },
        },
    },
        \\diff --git a/a b/a
        \\--- a/a
        \\+++ b/a
        \\@@ -1 +1 @@
        \\-a
        \\\ No newline at end of file
        \\+a
    );
}

test "tricky new names" {
    // $ find a/ b/ -type f
    // a/a b/a
    // b/a b/a b/a
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "a/a b/a",
            .status = .new,
        },
        .{
            .path = "b/a b/a b/a",
            .status = .new,
        },
    },
        \\diff --git a/a/a b/a b/a/a b/a
        \\new file mode 100644
        \\index 0000000..e69de29
        \\diff --git a/b/a b/a b/a b/b/a b/a b/a
        \\new file mode 100644
        \\index 0000000..e69de29
    );
}

test "tricky rename" {
    // $ touch "a/a b/a"
    // $ git add .
    // $ mv "a/a b/a" "b/a b/a b/"
    // $ git add .
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "a/a b/a",
            .status = .{ .renamed_to = 1 },
        },
        .{
            .path = "b/a b/a b/a",
            .status = .new,
        },
    },
        \\diff --git a/a/a b/a b/b/a b/a b/a
        \\similarity index 100%
        \\rename from a/a b/a
        \\rename to b/a b/a b/a
    );
}

test "renamed and modified" {
    try expectDiff(&[_]ChangeSet.File{
        .{
            .path = "src/ChangeSet_test.zig",
            .status = .{ .renamed_to = 1 },
        },
        .{
            .path = "src/DiffParser_test.zig",
            .status = .{
                .modified_ranges = &[_][2]u32{
                    .{ 17, 17 },
                },
            },
        },
    },
        \\diff --git a/src/ChangeSet_test.zig b/src/DiffParser_test.zig
        \\similarity index 84%
        \\rename from src/ChangeSet_test.zig
        \\rename to src/DiffParser_test.zig
        \\index 32cd768..adbf07a 100644
        \\--- a/src/ChangeSet_test.zig
        \\+++ b/src/DiffParser_test.zig
        \\@@ -14,7 +14,7 @@ test ChangeSet {
        \\     , &[_]ChangeSet.File{
        \\         .{
        \\             .path = "foo",
        \\-            .changes = .{
        \\+            .status = .{
        \\                 .modified_ranges = &[_][2]u32{
        \\                     .{ 1, 1 },
        \\                 },
    );
}

test "test.patch" {
    // To debug the parser on specific files, add a new `test.patch` file at the root of the
    // repository and run this test.
    const diff_buffer = std.fs.cwd().readFileAlloc(
        std.testing.allocator,
        "test.patch",
        1024 * 1024,
    ) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer std.testing.allocator.free(diff_buffer);

    var change_set = try parseTestDiff(&[_][]const u8{diff_buffer});
    change_set.deinit();
}

// -------------------------------------------------------------------------------------------------
// MARK: Helpers

/// Expects the parsed `diff` to match `expected`.
///
/// Bitsets in `ChangeSet.File.status` are converted to ranges before comparison.
inline fn expectDiff(expected: []const ChangeSet.File, diff: []const u8) !void {
    var split_buffer: test_helpers.SplitBufferIterator = .init(diff);
    defer split_buffer.deinit();
    errdefer split_buffer.debug();

    while (split_buffer.next()) |parts| {
        var change_set = try parseTestDiff(parts);
        defer change_set.deinit();

        try convertBitsetsToRanges(&change_set);

        try std.testing.expectEqualDeep(expected, change_set.files.items);
    }
}

/// Parses a `diff` into a `ChangeSet` in the `std.testing.allocator`.
inline fn parseTestDiff(diff: []const []const u8) !ChangeSet {
    var change_set: ChangeSet = .init(std.testing.allocator);
    errdefer change_set.deinit();

    var scratch: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer scratch.deinit();

    var parser: DiffParser = .init(&change_set, scratch.allocator());
    defer parser.deinit();

    for (diff) |slice| {
        try parser.parse(slice);
    }
    try parser.finish();

    return change_set;
}

/// Converts any `modified_lines` `ChangeSet.File.status` stored as bitsets into `modified_ranges`.
fn convertBitsetsToRanges(change_set: *ChangeSet) error{OutOfMemory}!void {
    const scratch = std.testing.allocator;

    var ranges: std.ArrayList([2]u32) = .empty;
    defer ranges.deinit(scratch);

    for (change_set.files.items) |*file| {
        var bitset = switch (file.status) {
            .modified_lines => |*bitset| bitset,
            else => continue,
        };
        errdefer bitset.deinit(change_set.allocator);

        var iterator = bitset.iterator(.{ .kind = .set, .direction = .forward });

        while (iterator.next()) |i| {
            const v: u32 = @intCast(i);

            if (ranges.items.len == 0 or ranges.items[ranges.items.len - 1][1] != v - 1) {
                try ranges.append(scratch, .{ v, v });
            } else {
                ranges.items[ranges.items.len - 1][1] = v;
            }
        }

        bitset.deinit(change_set.allocator);
        file.status = .{
            .modified_ranges = try change_set.allocator.dupe([2]u32, ranges.items),
        };
        ranges.clearRetainingCapacity();
    }
}
