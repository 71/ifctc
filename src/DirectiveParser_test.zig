const DirectiveParser = @import("DirectiveParser.zig");
const std = @import("std");
const test_helpers = @import("test_helpers.zig");

test "parse simple directive" {
    try expectDirectives(&[_]DirectiveParser.ParsedDirective{
        .{
            .directive_ty = .if_change,
            .args = &[_][]const u8{
                "label",
            },
            .start_line = 2,
        },
        .{
            .directive_ty = .then_change,
            .args = &[_][]const u8{
                "file.rs:l1",
                ":self_label",
            },
            .start_line = 5,
        },
    },
        \\abc
        \\// LINT.IfChange(label)
        \\foo
        \\bar
        \\// LINT.ThenChange(file.rs:l1, :self_label)
    );
}

test "fuzz" {
    const Context = struct {};

    const fuzz = struct {
        fn fuzz(_: Context, input: []const u8) anyerror!void {
            var parser: DirectiveParser = .init(std.testing.allocator, null);
            defer parser.deinit();

            var stuck_checker: test_helpers.StuckChecker = .{};

            var buf = input;
            while (buf.len > 0) {
                try stuck_checker.check();
                _ = try parser.next(&buf);
            }
        }
    }.fuzz;

    try std.testing.fuzz(Context{}, fuzz, .{});
}

fn expectDirectives(expected: []const DirectiveParser.ParsedDirective, buffer: []const u8) !void {
    var split_buffer: test_helpers.SplitBufferIterator = .init(buffer);
    defer split_buffer.deinit();
    errdefer split_buffer.debug();

    while (split_buffer.next()) |parts| {
        var stuck_checker: test_helpers.StuckChecker = .{};

        var parser: DirectiveParser = .init(std.testing.allocator, null);
        defer parser.deinit();

        var part_i: usize = 0;
        var directive_i: usize = 0;
        var part: []const u8 = parts[0];

        while (true) {
            try stuck_checker.check();

            const directive = parser.next(&part) catch |err| switch (err) {
                error.WantMore => {
                    part_i += 1;
                    try std.testing.expect(part_i < parts.len);
                    part = parts[part_i];
                    continue;
                },
                else => return err,
            };

            if (directive) |d| {
                try std.testing.expect(directive_i < expected.len);
                try std.testing.expectEqualDeep(expected[directive_i], d);

                directive_i += 1;
            } else if (part_i + 1 < parts.len) {
                part_i += 1;
                part = parts[part_i];
            } else {
                break;
            }
        }
    }
}
