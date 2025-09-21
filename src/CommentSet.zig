//! A set of comment prefixes which can be encountered in a language.
const CommentSet = @This();

const std = @import("std");

/// The non-empty slice of comment prefixes.
line_comments: []const []const u8,
/// The length of the shortest comment prefix in `line_comments`.
line_comments_min_len: u8,

/// Returns the list of comment prefixes for the given filename's extension if known.
pub fn forExtension(filename: []const u8) ?*const CommentSet {
    const dot_index = std.mem.lastIndexOfScalar(u8, filename, '.') orelse return null;
    if (dot_index + 1 >= filename.len) return null;

    const extension = filename[dot_index + 1 ..];
    const first_char = std.ascii.toLower(extension[0]);
    if (first_char < 'a' or first_char > 'z') return null;

    const entries = comments_per_extension[@intCast(first_char - 'a')];
    for (entries) |*entry| {
        // The stored path doesn't include the first character.
        if (std.ascii.eqlIgnoreCase(entry.ext, extension[1..])) return &entry.comments;
    }
    return null;
}

test forExtension {
    try std.testing.expectEqualDeep(forExtension("test.c"), &CommentSet{
        .line_comments = &[_][]const u8{"//"},
        .line_comments_min_len = 2,
    });
    try std.testing.expectEqualDeep(forExtension("test.rs"), &CommentSet{
        .line_comments = &[_][]const u8{"//"},
        .line_comments_min_len = 2,
    });
    try std.testing.expectEqualDeep(forExtension("test.py"), &CommentSet{
        .line_comments = &[_][]const u8{"#"},
        .line_comments_min_len = 1,
    });
}

/// Returns the length of the first comment with which `buffer`, which must be non-empty, starts.
pub fn starts(self: *const CommentSet, buffer: []const u8) ?usize {
    std.debug.assert(buffer.len > 0);
    if (buffer.len == 0) return null;

    for (self.line_comments) |comment| {
        if (buffer.len >= comment.len and
            std.mem.eql(u8, buffer[0..comment.len], comment))
        {
            return comment.len;
        }
    }
    return null;
}

/// Returns the length of the first comment with which `buffer`, which must be non-empty, ends.
pub fn terminates(self: *const CommentSet, buffer: []const u8) ?usize {
    std.debug.assert(buffer.len > 0);
    if (buffer.len == 0) return null;

    for (self.line_comments) |comment| {
        if (buffer.len >= comment.len and
            std.mem.eql(u8, buffer[buffer.len - comment.len ..], comment))
        {
            return comment.len;
        }
    }
    return null;
}

// -------------------------------------------------------------------------------------------------
// MARK: Helpers

const extensions_per_comment = [_]struct { comment: []const u8, extensions: []const []const u8 }{
    .{
        .comment = "//",
        .extensions = &[_][]const u8{ "c", "cc", "cpp", "c++", "go", "h", "hh", "hpp", "h++", "json", "jsonc", "json5", "js", "ts", "jsx", "tsx", "rs", "zig", "zon" },
    },
    .{
        .comment = "#",
        .extensions = &[_][]const u8{ "bazel", "py", "sh", "toml", "yaml", "yml" },
    },
    .{
        .comment = "<!--",
        .extensions = &[_][]const u8{ "html", "md", "xml" },
    },
};
const comments_per_extension = blk: {
    const Entry = struct { ext: []const u8, comments: CommentSet };
    var first_char_to_entries = [1][]const Entry{&[0]Entry{}} ** 26;

    for (extensions_per_comment) |entry| {
        next_ext: for (entry.extensions) |ext| {
            std.debug.assert(ext.len > 0);
            std.debug.assert(std.ascii.isLower(ext[0]));

            const entries = &first_char_to_entries[@intCast(ext[0] - 'a')];
            const ext_rest = ext[1..];

            const insertion_index = for (entries.*, 0..) |*e, i| {
                switch (std.mem.order(u8, ext_rest, e.ext)) {
                    .eq => {
                        e.comments.line_comments = e.comments.line_comments ++ &[1][]const u8{entry.comment};
                        e.comments.line_comments_min_len = @min(e.comments.line_comments_min_len, @as(u8, @intCast(entry.comment.len)));
                        continue :next_ext;
                    },
                    .gt => continue,
                    .lt => break i,
                }
            } else entries.len;

            const entries_before = entries.*[0..insertion_index];
            const entries_after = entries.*[insertion_index..];
            const new_entry: Entry = .{
                .ext = ext_rest,
                .comments = .{
                    .line_comments = &[1][]const u8{entry.comment},
                    .line_comments_min_len = @as(u8, @intCast(entry.comment.len)),
                },
            };

            entries.* = entries_before ++ &[1]Entry{new_entry} ++ entries_after;
        }
    }

    break :blk first_char_to_entries;
};
