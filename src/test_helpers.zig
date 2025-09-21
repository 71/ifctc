const std = @import("std");

/// An iterator which provides, via `next()` several ways to split a buffer into its subslices.
pub const SplitBufferIterator = struct {
    buffer: []const u8,
    slices: std.ArrayList([]const u8),
    iter: u8 = 0,

    pub fn init(buffer: []const u8) SplitBufferIterator {
        return .{
            .buffer = buffer,
            .slices = std.ArrayList([]const u8).initCapacity(std.testing.allocator, @max(buffer.len, 1)) catch unreachable,
        };
    }

    pub fn deinit(self: *SplitBufferIterator) void {
        self.slices.deinit(std.testing.allocator);
    }

    /// Returns the combination of slices of the input `buffer` number `n`.
    pub fn at(self: *SplitBufferIterator, n: u8) ?[]const []const u8 {
        if (self.iter != 0) return null; // Already called this function once.
        std.debug.assert(sliceFor(self.buffer, &self.slices, n));
        self.iter = n + 1;
        return self.slices.items;
    }

    /// Returns the next combination of slices of the input `buffer`, or null if there are no more
    /// combinations.
    pub fn next(self: *SplitBufferIterator) ?[]const []const u8 {
        if (!sliceFor(self.buffer, &self.slices, self.iter)) return null;
        self.iter += 1;
        return self.slices.items;
    }

    /// Prints debug information about the current iteration.
    ///
    /// Usage:
    ///
    /// ```
    /// errdefer iterator.debug();
    /// ```
    pub fn debug(self: *const SplitBufferIterator) void {
        std.debug.print("\nBuffer parts: ", .{});

        const slices = self.slices.items;
        const upper = @min(slices.len, 5);

        for (0..upper) |i| {
            var subslice = slices[i];

            if (subslice.len < 6) {
                std.debug.print("\"{s}\"", .{subslice});
            } else {
                subslice = subslice[0..6];

                std.debug.print("\"{s}...\" ({} more bytes)", .{ subslice, slices[i].len - 6 });
            }
            std.debug.print("{s}", .{if (i == slices.len - 1) "" else ", "});
        }

        if (upper < slices.len) {
            std.debug.print("... ({} more parts)", .{slices.len - upper});
        }
        std.debug.print("\n\nReplace `iterator.next()` with `iterator.at({})` to debug.\n", .{self.iter - 1});
    }
};

/// Modifies `slices` such as that, when concatenated, we get back `buffer`.
///
/// `iter` corresponds to a specific way to split `buffer`. If all the ways were tried, returns
/// false.
fn sliceFor(buffer: []const u8, slices: *std.ArrayList([]const u8), iter: u8) bool {
    slices.clearRetainingCapacity();

    if (buffer.len == 0 and iter > 0) return false;
    if (buffer.len == 1 and iter > 3) return false;

    switch (iter) {
        // Whole slice.
        0 => slices.appendAssumeCapacity(buffer),

        // One byte at a time.
        1 => for (buffer) |*c| {
            slices.appendAssumeCapacity(c[0..1]);
        },

        // Empty first/last part.
        2 => {
            slices.appendAssumeCapacity("");
            slices.appendAssumeCapacity(buffer);
        },
        3 => {
            slices.appendAssumeCapacity(buffer);
            slices.appendAssumeCapacity("");
        },

        // Split first/last characters.
        4 => {
            slices.appendAssumeCapacity(buffer[0..1]);
            slices.appendAssumeCapacity(buffer[1..]);
        },
        5 => {
            slices.appendAssumeCapacity(buffer[0 .. buffer.len - 1]);
            slices.appendAssumeCapacity(buffer[buffer.len - 1 ..]);
        },
        6 => {
            slices.appendAssumeCapacity(buffer[0..1]);
            slices.appendAssumeCapacity(buffer[1 .. buffer.len - 1]);
            slices.appendAssumeCapacity(buffer[buffer.len - 1 ..]);
        },

        // Split in the middle.
        7 => {
            slices.appendAssumeCapacity(buffer[0 .. buffer.len / 2]);
            slices.appendAssumeCapacity(buffer[buffer.len / 2 ..]);
        },
        8 => {
            slices.appendAssumeCapacity(buffer[0 .. buffer.len / 2]);
            slices.appendAssumeCapacity(""); // With empty slice.
            slices.appendAssumeCapacity(buffer[buffer.len / 2 ..]);
        },

        else => return false,
    }
    return true;
}

test SplitBufferIterator {
    var split_buffer: SplitBufferIterator = .init("012345");
    defer split_buffer.deinit();

    try std.testing.expectEqualDeep(&[_][]const u8{"012345"}, split_buffer.next());
    try std.testing.expectEqualDeep(&[_][]const u8{ "0", "1", "2", "3", "4", "5" }, split_buffer.next());

    try std.testing.expectEqualDeep(&[_][]const u8{ "", "012345" }, split_buffer.next());
    try std.testing.expectEqualDeep(&[_][]const u8{ "012345", "" }, split_buffer.next());

    try std.testing.expectEqualDeep(&[_][]const u8{ "0", "12345" }, split_buffer.next());
    try std.testing.expectEqualDeep(&[_][]const u8{ "01234", "5" }, split_buffer.next());
    try std.testing.expectEqualDeep(&[_][]const u8{ "0", "1234", "5" }, split_buffer.next());

    try std.testing.expectEqualDeep(&[_][]const u8{ "012", "345" }, split_buffer.next());
    try std.testing.expectEqualDeep(&[_][]const u8{ "012", "", "345" }, split_buffer.next());

    try std.testing.expectEqualDeep(null, split_buffer.next());
}

/// Small helper used to detect infinite loops in tests.
pub const StuckChecker = struct {
    iterations: u16 = 0,
    max: u32 = 1_000,

    /// Returns `error.Stuck` if this function was called more than `max` times.
    pub fn check(self: *StuckChecker) error{Stuck}!void {
        if (self.iterations >= self.max) {
            return error.Stuck;
        }
        self.iterations += 1;
    }
};

test StuckChecker {
    var checker: StuckChecker = .{ .max = 2 };

    try checker.check();
    try checker.check();
    try std.testing.expectEqual(error.Stuck, checker.check());
}
