const std = @import("std");

/// An append-only list of items stored in a singly linked list of chunks of a given size.
///
/// This data structure works particurarly well with arena/bump allocators, as it never "grows",
/// while being more cache-friendly than a linked list of individual items.
///
/// The first chunk is always stored inline.
pub fn ChunkList(comptime T: type, comptime chunk_size: u8) type {
    comptime std.debug.assert(chunk_size > 0);

    const Chunk = struct {
        const Self = @This();

        /// If `<= chunk_size`, the number of items in this chunk. If `> chunk_size`, a pointer to
        /// the next chunk (in which case `len()` is `chunk_size`).
        ///
        /// Instead of relying on the zero page being null in most platforms, we could use a tagged
        /// pointer here (require `T` to have an alignment >= 2, and use the first bit as a tag).
        next_or_len: ?*anyopaque,

        /// The actual items in the chunk. These are undefined until `tryAppend()` is called, which
        /// will also increase `next_or_len`.
        items: [chunk_size]T = undefined,

        /// Returns the number of items in this chunk, such that `items[0..len()]` is initialized.
        pub fn len(self: *const Self) usize {
            return @min(chunk_size, @intFromPtr(self.next_or_len));
        }

        /// Tries to append an item to this chunk, returning a pointer to the item if it was
        /// appended, or `null` if the chunk is full.
        pub fn tryAppend(self: *Self, item: T) ?*T {
            const next_as_len = @intFromPtr(self.next_or_len);
            if (next_as_len >= chunk_size) {
                return null;
            }
            self.items[next_as_len] = item;
            self.next_or_len = @ptrFromInt(next_as_len + 1);
            return &self.items[next_as_len];
        }

        /// Returns an iterator over a linked list of chunks. `C` is `*Chunk` or `*const Chunk`, `R`
        /// is `*T` or `*const T`.
        pub fn Iterator(comptime C: type, comptime R: type) type {
            return struct {
                chunk: C,
                index: u8,

                pub fn next(self: *@This()) ?R {
                    const next_as_len = @intFromPtr(self.chunk.next_or_len);

                    if (self.index >= next_as_len) {
                        @branchHint(.unlikely);

                        return null;
                    }

                    if (self.index < chunk_size) {
                        const result = &self.chunk.items[self.index];
                        self.index += 1;
                        return result;
                    }

                    const next_chunk: C = @ptrCast(@alignCast(self.chunk.next_or_len));

                    // Only the first chunk is initialized with `.next_or_len = 0`; all next chunks
                    // are initialized with `.next_or_len = 1` when created in `append()`.
                    std.debug.assert(next_chunk.len() > 0);

                    self.chunk = next_chunk;
                    self.index = 1;

                    return &next_chunk.items[0];
                }
            };
        }
    };

    return struct {
        const Self = @This();

        /// The first chunk, stored inline for small lists.
        first: Chunk = .{ .next_or_len = null },
        /// A pointer to the last chunk, or `null` if `first` is the only chunk.
        last_if_not_first: ?*Chunk = null,

        pub const empty: Self = .{};

        /// Frees all chunks in the list. Note that individual items are expected to be freed by the
        /// caller (see `iter()`).
        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            var next_chunk = self.first.next_or_len;
            while (next_chunk) |chunk| {
                const chunk_as_len = @intFromPtr(chunk);
                if (chunk_as_len <= chunk_size) {
                    break; // Last chunk.
                }
                const chunk_ptr: *Chunk = @ptrCast(@alignCast(chunk));
                next_chunk = chunk_ptr.next_or_len;
                allocator.destroy(chunk_ptr);
            }
            self.first.next_or_len = null;
            self.last_if_not_first = null;
        }

        pub fn copyToOwnedSlice(self: *const Self, allocator: std.mem.Allocator) error{OutOfMemory}![]T {
            const slice = try allocator.alloc(T, self.len());
            var index: usize = 0;
            var chunk: *const Chunk = &self.first;
            while (true) {
                const next_as_len = @intFromPtr(chunk.next_or_len);
                const to_copy = @min(next_as_len, chunk_size);
                @memcpy(slice[index .. index + to_copy], chunk.items[0..to_copy]);
                index += to_copy;
                if (next_as_len <= chunk_size) {
                    std.debug.assert(index == slice.len);
                    break;
                }
                chunk = @ptrCast(@alignCast(chunk.next_or_len));
            }
            return slice;
        }

        /// Appends `item` to the list, returning a pointer to the appended item.
        ///
        /// The pointer is guaranteed to be stable (even if the list grows) as long as the pointer
        /// to `self` is stable.
        pub fn append(self: *Self, allocator: std.mem.Allocator, item: T) error{OutOfMemory}!*T {
            const last = self.last_if_not_first orelse &self.first;
            if (last.tryAppend(item)) |result| return result;

            const new_chunk = try allocator.create(Chunk);
            new_chunk.* = .{ .next_or_len = @ptrFromInt(1) };
            new_chunk.items[0] = item;
            last.next_or_len = new_chunk;
            self.last_if_not_first = new_chunk;
            return &new_chunk.items[0];
        }

        /// Returns whether the list is empty.
        pub fn isEmpty(self: *const Self) bool {
            return self.first.next_or_len == null;
        }

        /// Returns the number of items in the list in O(n).
        pub fn len(self: *const Self) usize {
            var count: usize = 0;
            var chunk: *const Chunk = &self.first;
            while (true) {
                const next_as_len = @intFromPtr(chunk.next_or_len);
                if (next_as_len <= chunk_size) {
                    return count + next_as_len;
                }
                count += chunk_size;
                chunk = @ptrCast(@alignCast(chunk.next_or_len));
            }
        }

        pub const Iterator = Chunk.Iterator(*Chunk, *T);
        pub const ConstIterator = Chunk.Iterator(*const Chunk, *const T);

        pub fn iterator(self: *Self) Iterator {
            return .{ .chunk = &self.first, .index = 0 };
        }

        pub fn constIterator(self: *const Self) ConstIterator {
            return .{ .chunk = &self.first, .index = 0 };
        }
    };
}

/// Tests a `ChunkList` by appending `n` items then iterating over the list, making sure that
/// invariants are held along the way.
fn testChunkList(comptime n: usize) !void {
    // Append.
    var list: ChunkList(u32, 16) = .empty;
    defer list.deinit(std.testing.allocator);

    for (0..n) |i| {
        const appended = try list.append(std.testing.allocator, @intCast(i));
        try std.testing.expectEqual(i, appended.*);
    }

    // Iterate.
    var iter = list.iterator();
    var expected: u32 = 0;

    while (iter.next()) |value| {
        try std.testing.expectEqual(expected, value.*);
        expected += 1;
    }
    try std.testing.expectEqual(expected, n);

    // Copy to slice.
    const slice = try list.copyToOwnedSlice(std.testing.allocator);
    defer std.testing.allocator.free(slice);

    try std.testing.expectEqual(n, slice.len);
    for (slice, 0..) |value, i| {
        const expected_value: u32 = @intCast(i);
        try std.testing.expectEqual(expected_value, value);
    }
}

test "empty ChunkList" {
    try testChunkList(0);
}
test "ChunkList with 1 item" {
    try testChunkList(1);
}
test "ChunkList with >1, but <chunk_size items" {
    try testChunkList(8);
}
test "ChunkList with exactly chunk_size items" {
    try testChunkList(16);
}
test "ChunkList with exactly 2*chunk_size items" {
    try testChunkList(32);
}
test "ChunkList with >2*chunk_size, but <3*chunk_size items" {
    try testChunkList(50);
}
