//! SIMD-powered string utilities.
const std = @import("std");

// -------------------------------------------------------------------------------------------------
// MARK: countNewlines

/// Returns the number of newlines in `haystack`.
pub fn countNewlines(haystack: []const u8) usize {
    // We could use `std.simd.suggestVectorLength(u8)` below, but it gives a fairly conservative
    // estimate so we manually use 512-bit vectors and then 128-bit vectors for the computation
    // instead.
    const nl512_len = 512 / 8;
    const nl128_len = 128 / 8;
    const V512 = @Vector(nl512_len, u8);
    const V128 = @Vector(nl128_len, u8);
    const nl512: V512 = @splat('\n');
    const nl128: V128 = @splat('\n');

    var s: []const u8 = haystack;
    var count: usize = 0;

    while (s.len >= nl512_len) {
        const vec: V512 = s[0..nl512_len].*;
        const is_newline: @Vector(nl512_len, bool) = vec == nl512;

        count += std.simd.countTrues(is_newline);
        s = s[nl512_len..];
    }
    while (s.len >= nl128_len) {
        const vec: V128 = s[0..nl128_len].*;
        const is_newline: @Vector(nl128_len, bool) = vec == nl128;

        count += std.simd.countTrues(is_newline);
        s = s[nl128_len..];
    }
    for (s) |c| {
        count += if (c == '\n') 1 else 0;
    }

    return count;
}

test countNewlines {
    try std.testing.expectEqual(5, countNewlines("1\n2\n3\n4\n5\n"));

    const only_newlines = "\n" ** 1200;
    try std.testing.expectEqual(1200, countNewlines(only_newlines));

    const no_newlines = "a" ** 1200;
    try std.testing.expectEqual(0, countNewlines(no_newlines));

    const mixed = "abc\n" ** 1200;
    try std.testing.expectEqual(1200, countNewlines(mixed));
}

// -------------------------------------------------------------------------------------------------
// MARK: indexOf

const index_of_vector_len = std.simd.suggestVectorLength(u8) orelse 128;

/// Returns the index of the first occurrence of `needle` in `haystack`, or null if not found.
pub fn indexOf(haystack: []const u8, comptime needle: []const u8) ?usize {
    comptime {
        std.debug.assert(needle.len > 2);

        // This function is intended to be used with needles whose starting and ending bytes are not
        // present in the rest of the needle.
        std.debug.assert(std.mem.indexOfScalar(u8, needle[1..], needle[0]) == null);
        std.debug.assert(std.mem.indexOfScalar(u8, needle[0 .. needle.len - 1], needle[needle.len - 1]) == null);
    }

    const fns = struct {
        fn is_match(middle_bytes: [needle.len - 2]u8) bool {
            return std.mem.eql(u8, &middle_bytes, needle[1 .. needle.len - 1]);
        }

        fn find_linear(hs: []const u8, offset: usize) ?usize {
            return std.mem.indexOfPosLinear(u8, hs, offset, needle);
        }
    };

    return indexOfSurrounded(
        haystack,
        needle[0],
        needle[needle.len - 1],
        needle.len,
        fns.is_match,
        fns.find_linear,
    );
}

test indexOf {
    try std.testing.expectEqual(0, indexOf("abcd" ** 200, "abcd"));
    try std.testing.expectEqual(null, indexOf(" " ** 1000, "abcd"));

    try std.testing.expectEqual(100, indexOf(" " ** 100 ++ "abcd" ** 10, "abcd"));
    try std.testing.expectEqual(1000, indexOf(" " ** 1000 ++ "abcd" ** 10, "abcd"));
}

test "indexOf split chunks" {
    const V = index_of_vector_len;

    inline for (0..8) |i| {
        inline for ([2]bool{ false, true }) |inv| {
            const offset = if (inv) V - i else V + i;
            const string = " " ** offset ++ "abcdxyz";

            try std.testing.expectEqual(offset, indexOf(string, "abcd"));

            const string_followed_by_chunks = string ++ " " ** (V * 10);

            try std.testing.expectEqual(offset, indexOf(string_followed_by_chunks, "abcd"));
        }
    }
}

// -------------------------------------------------------------------------------------------------
// MARK: indexOfSurrounded

/// Returns the first index in `haystack` such that:
///
/// - `haystack[index] == start`
/// - `haystack[index + len - 1] == end`
/// - `is_match(haystack[index + 1 .. index + len - 1])`
///
/// Falling back to `find_linear(haystack, offset)` for bytes which cannot be
/// scanned with SIMD.
pub fn indexOfSurrounded(
    haystack: []const u8,
    comptime start: u8,
    comptime end: u8,
    comptime len: u8,
    comptime is_match: fn (middle_bytes: [len - 2]u8) bool,
    comptime find_linear: fn (haystack: []const u8, offset: usize) ?usize,
) ?usize {
    // This is essentially the same logic as `indexOf()`, but without checking
    // the middle bytes of the needle.
    comptime {
        std.debug.assert(len > 2);
        std.debug.assert(start != end);
    }

    const vector_len = index_of_vector_len;
    const shift_by = len - 1;

    const first: @Vector(vector_len, u8) = @splat(start);
    const last: @Vector(vector_len, u8) = @splat(end);

    var s: []const u8 = haystack;
    var offset: usize = 0;

    while (s.len >= vector_len + shift_by) {
        const start_block: @Vector(vector_len, u8) = s[0..vector_len].*;
        const end_block: @Vector(vector_len, u8) = s[shift_by .. shift_by + vector_len].*;
        const is_start: @Vector(vector_len, bool) = start_block == first;
        const is_end: @Vector(vector_len, bool) = end_block == last;

        var possible_starts: @Vector(vector_len, bool) = is_start & is_end;

        while (std.simd.firstTrue(possible_starts)) |idx| {
            if (is_match(haystack[offset + idx ..][1 .. len - 1].*)) {
                return offset + idx;
            }

            possible_starts[idx] = false;
        }

        offset += vector_len;
        s = s[vector_len..];
    }

    return find_linear(haystack, offset);
}

// -------------------------------------------------------------------------------------------------
// MARK: indexOfAny

/// Returns the index of the first occurrence of any byte in `needles` in `haystack`, or null if not
/// found.
pub fn indexOfAny(haystack: []const u8, comptime needles: []const u8) ?usize {
    comptime {
        std.debug.assert(needles.len > 1 and needles.len < 4);

        // This function is intended to be used with small needles that are all distinct.
        for (needles[1..], 1..) |v, i| {
            std.debug.assert(std.mem.indexOfScalar(u8, needles[0..i], v) == null);
        }
    }

    const vector_len = index_of_vector_len;
    var offset: usize = 0;

    while (haystack.len >= offset + vector_len) {
        const chunk: @Vector(vector_len, u8) = haystack[offset..][0..vector_len].*;
        var smallest_index: usize = vector_len;

        inline for (needles) |needle| {
            if (std.simd.firstIndexOfValue(chunk, needle)) |index| {
                smallest_index = @min(smallest_index, index);
            }
        }

        if (smallest_index != vector_len) {
            return offset + smallest_index;
        }

        offset += vector_len;
    }

    return std.mem.indexOfAnyPos(u8, haystack, offset, needles);
}

test indexOfAny {
    try std.testing.expectEqual(0, indexOfAny("abcd", "ax"));
    try std.testing.expectEqual(1, indexOfAny(" xyz", "ax"));
    try std.testing.expectEqual(null, indexOfAny("abcd", "AX"));

    try std.testing.expectEqual(1000, indexOfAny(" " ** 1000 ++ "abcd", "ax"));
    try std.testing.expectEqual(1001, indexOfAny(" " ** 1000 ++ " xyz", "ax"));
    try std.testing.expectEqual(null, indexOfAny(" " ** 1000 ++ "abcd", "AX"));
}
