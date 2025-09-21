//! A sans-IO parser for "LINT.IfChange" / "LINT.ThenChange" directives.
//!
//! No exact format or reference implementation exists publicly, so some guesses
//! have been made on how the parser should behave. Here are a few example
//! usages in public code:
//!
//! - https://dart.googlesource.com/tools.git/+/a5c30922/pkgs/unified_analytics/pubspec.yaml:
//!   References a relative path.
//!
//! - https://chromium.googlesource.com/chromium/src/base/+/master/android/java/src/org/chromium/base/SysUtils.java (a53509c189124da2e7d5b5db066d3437be326ce9):
//!   References a repo-absolute path with //base/.
//!
//! - https://source.chromium.org/chromium/chromium/src/+/main:components/policy/proto/device_management_backend.proto?q=LINT.ThenChange&ss=chromium:
//!   References a repo-absolute path with //, multiple paths separate by "," (no space, no newline),
//!   has label in IfChange (no quote).
//!
//! - https://source.chromium.org/chromium/chromium/src/+/main:components/sync/protocol/sync_enums.proto;l=230?q=LINT.ThenChange&ss=chromium%2Fchromium%2Fsrc:components%2F:
//!   References a repo-absolute path with "/".
//!
//! - https://source.chromium.org/chromium/chromium/src/+/main:components/sync/engine/update_handler.h;l=34?q=LINT.&ss=chromium%2Fchromium%2Fsrc:components%2F&start=51:
//!   Is not at the start of a comment, but after some text.
//!
//! - https://source.chromium.org/chromium/chromium/src/+/main:components/external_intents/android/java/src/org/chromium/components/external_intents/ExternalNavigationHandler.java;l=408?q=LINT.&ss=chromium%2Fchromium%2Fsrc:components%2F&start=61:
//!   References a label in the same file.
//!
//! - https://fuchsia.dev/fuchsia-src/development/source_code/presubmit_checks:
//!   `LINT.` in a block comment (i.e. scanning only comment lines is not enough).
//!
//! - https://source.chromium.org/search?q=LINT.ThenChange&sq=&ss=chromium: more examples.
//!
//! Therefore:
//!
//! - Directives start with `LINT.IfChange` and end with `LINT.ThenChange`.
//!
//! - `IfChange` may contain a label (no quotes, no spaces) in parentheses.
//!
//! - `ThenChange` may contain multiple paths separated by ",".
//!   * We additionally accept spaces around ",", including newlines.
//!
//! - `ThenChange` can refer to labels using ":".
//!   * Including in the same file by specifying no path.
//!
//! - Directives can appear anywhere, without scanning for comments. To avoid false positives, we
//!   require "LINT" to be preceded by a space (which allows documentation to write "LINT" or `LINT`
//!   to avoid being scanned).
const DirectiveParser = @This();

test {
    _ = @import("DirectiveParser_test.zig");
}

const CommentSet = @import("CommentSet.zig");
const std = @import("std");
const strings = @import("strings.zig");

const State = union(enum) {
    /// Scanning for the start of a lint directive.
    scan,

    /// Found the start of a lint directive; parsing "LINT.".
    expect_lint: []const u8,

    /// Found " LINT."; expecting the first character of a directive name.
    expect_directive_start,

    /// Found " LINT."; expecting a directive name.
    expect_directive: struct {
        ty: ParsedDirective.Type,
        expected: []const u8,
    },

    /// Found a directive; parsing its arguments.
    parse_directive_args: struct {
        ty: ParsedDirective.Type,

        /// The line where the directive began, or 0 if no "(" has been seen yet.
        start_line: u32,

        // Rest of the state is in `parsed_directive_args` and
        // `current_directive_arg` to reuse allocations across directives.
    },
};

/// Result of parsing a directive with `next()`.
pub const ParsedDirective = struct {
    pub const Type = enum { if_change, then_change };

    /// The type of the parsed directive.
    directive_ty: Type,
    /// Arguments of the directive. Each string is owned by the parser and invalidated on the next
    /// call to `next()`.
    args: []const []const u8,
    /// The 1-based line where the directive begins.
    start_line: u32,
};
pub const Error = error{
    /// Need more input to continue parsing.
    WantMore,
    /// The directive is malformed.
    InvalidDirective,
    /// Could not allocate a string.
    OutOfMemory,
};

line: u32 = 1,
comments: ?*const CommentSet = null,

allocator: std.mem.Allocator,
state: State = .scan,

parsed_directive_args: std.ArrayList([]const u8) = .empty,
current_directive_arg: std.ArrayList(u8) = .empty,

pub fn init(allocator: std.mem.Allocator, comments: ?*const CommentSet) DirectiveParser {
    return DirectiveParser{
        .allocator = allocator,
        .comments = comments,
    };
}

pub fn deinit(self: *DirectiveParser) void {
    for (self.parsed_directive_args.items) |arg| {
        self.allocator.free(arg);
    }
    self.parsed_directive_args.deinit(self.allocator);
    self.current_directive_arg.deinit(self.allocator);
}

/// Parses the next directive from `input_buffer`.
///
/// Returns:
/// - The parsed directive, if any.
/// - `null` if no directive was found in the buffer.
/// - `error.WantMore` if a directive may be present but more input is needed.
///
/// `buf` will be updated as it is consumed.
pub fn next(self: *DirectiveParser, buf: *[]const u8) Error!?ParsedDirective {
    sw: switch (self.state) {
        .scan => {
            @branchHint(.likely);

            const directive_start = self.findLintDirectiveStart(buf.*) orelse return null;
            buf.* = buf.*[directive_start..];

            self.state = .expect_directive_start;
            continue :sw self.state;
        },

        .expect_lint => |missing| {
            @branchHint(.unlikely);

            // If previous buf ended with "L" / "LI" / "LIN" / "LINT", see if this buf
            // forms a full "LINT.".
            const prefix_len = prefixLength(buf.*, missing) orelse {
                // Not a "LINT." directive.
                self.state = .scan;
                continue :sw self.state;
            };
            if (prefix_len < missing.len) {
                // Wait until we can see the whole "LINT.".
                @branchHint(.unlikely);

                self.state = .{ .expect_lint = missing[prefix_len..] };

                // We return `null` instead of `error.WantMore` as we're not sure we're dealing with
                // a directive yet.
                return null;
            }
            buf.* = buf.*[prefix_len..];

            self.state = .expect_directive_start;
            continue :sw self.state;
        },
        .expect_directive_start => {
            @branchHint(.unlikely);

            if (buf.len == 0) return null;

            const ty: ParsedDirective.Type, const expected: []const u8 = switch (buf.*[0]) {
                'I' => .{ .if_change, "fChange" },
                'T' => .{ .then_change, "henChange" },
                else => |char| {
                    // Unknown directive.
                    @branchHint(.unlikely);

                    self.state = .scan;
                    buf.* = buf.*[1..];

                    if (std.ascii.isAlphanumeric(char)) {
                        return error.InvalidDirective;
                    }
                    continue :sw self.state;
                },
            };
            buf.* = buf.*[1..];

            self.state = .{
                .expect_directive = .{ .ty = ty, .expected = expected },
            };
            continue :sw self.state;
        },
        .expect_directive => |state| {
            @branchHint(.unlikely);

            const prefix_len = prefixLength(buf.*, state.expected) orelse {
                // Unknown directive.
                @branchHint(.unlikely);

                self.state = .scan;
                return error.InvalidDirective;
            };
            buf.* = buf.*[prefix_len..];

            if (prefix_len < state.expected.len) {
                // Wait until we can see the whole directive type.
                @branchHint(.unlikely);

                self.state = .{
                    .expect_directive = .{
                        .ty = state.ty,
                        .expected = state.expected[prefix_len..],
                    },
                };
                return error.WantMore;
            }

            // We have a complete directive.
            self.state = .{
                .parse_directive_args = .{ .ty = state.ty, .start_line = 0 },
            };
            continue :sw self.state;
        },
        .parse_directive_args => |state| {
            // Getting here is neither likely nor unlikely; if parsing arguments we might repeatedly
            // get called, but most of the time we won't.
            if (buf.len == 0) return error.WantMore;

            // If `start_line == 0`, we haven't seen "(" yet.
            const start_line = blk: {
                if (state.start_line != 0) break :blk state.start_line;

                if (buf.*[0] != '(') {
                    // No arguments given. If this is a `ThenChange` (which requires arguments), let
                    // our caller handle the error.
                    self.state = .scan;

                    return .{
                        .directive_ty = state.ty,
                        .args = &[_][]const u8{},
                        .start_line = self.line,
                    };
                }

                // Clear previous arguments.
                for (self.parsed_directive_args.items) |arg| {
                    self.allocator.free(arg);
                }
                self.parsed_directive_args.clearRetainingCapacity();
                self.current_directive_arg.clearRetainingCapacity();

                self.state = .{
                    .parse_directive_args = .{ .ty = state.ty, .start_line = self.line },
                };
                buf.* = buf.*[1..];

                break :blk self.line;
            };

            try self.parseDirectiveArgs(buf);

            // We parsed all arguments.
            std.debug.assert(self.current_directive_arg.items.len == 0);
            self.state = .scan;

            return .{
                .directive_ty = state.ty,
                .args = self.parsed_directive_args.items,
                .start_line = start_line,
            };
        },
    }
}

/// Whether the character is accepted as whitespace before "LINT".
fn isAcceptedWhitespace(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n';
}

/// Returns the index of the first character after "LINT.". If `buffer` does not
/// contain "LINT.", returns null. If it might, `self.state` is updated and `error.WantMore`
/// returned.
fn findLintDirectiveStart(self: *DirectiveParser, buffer: []const u8) ?usize {
    // Pointer into `buffer` used by the loop below to search for "LINT".
    var buf = buffer;

    // Search for "LINT" in the buffer.
    while (strings.indexOf(buf, "LINT")) |lint_start| {
        self.line += @intCast(strings.countNewlines(buf[0..lint_start]));

        if (lint_start == 0 or !isAcceptedWhitespace(buf[lint_start - 1])) {
            @branchHint(.unlikely);

            // The "LINT" does not follow whitespace. Keep going. Skip 5 since "LINT" may not
            // start directly after "LINT".
            //
            // `@call(.always_tail)` fails to compile here, so we manually use `continue`:
            // https://github.com/ziglang/zig/issues/18189.
            buf = buf[lint_start + 5 ..];

            continue;
        }
        if (lint_start + 4 >= buf.len) {
            // The "LINT" is at the end of the buffer. The next buffer may continue it.
            self.state = .{ .expect_lint = "." };

            // We return `null` instead of `error.WantMore` as we're not sure we're dealing with
            // a directive yet.
            return null;
        }

        // The "LINT." is valid!
        const offset = buf.ptr - buffer.ptr;

        return offset + lint_start + 5;
    }
    if (buffer.len == 0) return null;

    // No lint directive in this chunk. Count newlines and keep going.
    self.line += @intCast(strings.countNewlines(buffer));

    // Although this chunk doesn't contain `LINT`, it might end with a partial `LINT`.
    const offset: u8 = switch (buffer[buffer.len - 1]) {
        ' ', '\n', '\t' => 0,
        'L' => 1,
        'I' => if (buffer.len > 2 and buffer[buffer.len - 2] == 'L') 2 else return null,
        'N' => if (buffer.len > 3 and buffer[buffer.len - 2] == 'I' and buffer[buffer.len - 3] == 'L')
            3
        else
            return null,
        else => return null,
    };
    if (offset > 0) switch (buffer[buffer.len - 1 - offset]) {
        ' ', '\n', '\t' => {},
        else => return null,
    };

    self.state = .{ .expect_lint = "LINT."[offset..] };
    return null;
}

/// Parses the arguments of the directive.
fn parseDirectiveArgs(self: *DirectiveParser, buf: *[]const u8) Error!void {
    while (true) {
        const arg_end = strings.indexOfAny(buf.*, "),\n") orelse {
            try self.current_directive_arg.appendSlice(self.allocator, buf.*);
            return error.WantMore;
        };
        const read = buf.*[0..arg_end];
        const char = buf.*[arg_end];

        buf.* = buf.*[arg_end + 1 ..];

        switch (char) {
            ')' => {
                try self.current_directive_arg.appendSlice(self.allocator, read);
                try self.finishDirectiveArg(.last);
                return;
            },
            ',' => {
                try self.current_directive_arg.appendSlice(self.allocator, read);
                try self.finishDirectiveArg(.not_last);
            },
            '\n' => {
                // Newline must follow a comma or opening parenthesis, i.e. the current argument must be
                // empty.
                if (self.current_directive_arg.items.len != 0) {
                    return error.InvalidDirective;
                }
                self.line += 1;
            },
            else => unreachable,
        }
    }
}

/// Finishes the current argument and appends it to `parsed_directive_args`.
fn finishDirectiveArg(
    self: *DirectiveParser,
    last: enum { last, not_last },
) error{ InvalidDirective, OutOfMemory }!void {
    var current_arg = self.current_directive_arg.items;

    // Skip leading whitespace and comments.
    while (current_arg.len > 0) {
        if (isAcceptedWhitespace(current_arg[0])) {
            current_arg = current_arg[1..];
        } else if (self.comments) |comments| {
            if (comments.starts(current_arg)) |len| {
                // The rest of the argument is a comment.
                current_arg = current_arg[len..];
            } else {
                break;
            }
        } else {
            break;
        }
    }

    // Skip trailing whitespace and comments.
    while (current_arg.len > 0) {
        if (isAcceptedWhitespace(current_arg[current_arg.len - 1])) {
            current_arg = current_arg[0 .. current_arg.len - 1];
        } else if (self.comments) |comments| {
            if (comments.terminates(current_arg)) |len| {
                // The start of the argument is a comment.
                current_arg = current_arg[0 .. current_arg.len - len];
            } else {
                break;
            }
        } else {
            break;
        }
    }

    if (current_arg.len == 0) {
        // If the argument is now empty, it's invalid.
        if (last == .last and self.parsed_directive_args.items.len > 0) {
            // ...unless it's the last argument, as we accept trailing commas.
            return;
        }
        return error.InvalidDirective;
    }

    try self.parsed_directive_args.ensureUnusedCapacity(self.allocator, 1);

    self.parsed_directive_args.appendAssumeCapacity(try self.allocator.dupe(u8, current_arg));
    self.current_directive_arg.clearRetainingCapacity();
}

/// Returns the largest value `n` such that `haystack[0..n] == prefix[0..n]`, or `null` if `prefix`
/// cannot be a prefix of `haystack`.
fn prefixLength(haystack: []const u8, prefix: []const u8) ?usize {
    const common_len = @min(haystack.len, prefix.len);

    if (!std.mem.eql(u8, haystack[0..common_len], prefix[0..common_len])) return null;

    return common_len;
}
