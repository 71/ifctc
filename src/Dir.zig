//! A wrapper around a read-only `std.fs.Dir` that can also represent a fake in-memory directory.
const Dir = @This();
const std = @import("std");

pub const File = struct {
    data: union(enum) {
        real: std.fs.File,
        in_memory: struct {
            contents: []const u8,
            chunk_size: usize,
        },
    },

    pub fn real(file: std.fs.File) File {
        return .{
            .data = .{ .real = file },
        };
    }

    pub fn inMemory(contents: []const u8, chunk_size: usize) File {
        return .{
            .data = .{ .in_memory = .{ .contents = contents, .chunk_size = chunk_size } },
        };
    }

    pub fn close(file: File) void {
        switch (file.data) {
            .real => |f| f.close(),
            .in_memory => {},
        }
    }

    pub fn read(self: *File, buffer: []u8) std.fs.File.ReadError!usize {
        return switch (self.data) {
            .real => |f| f.read(buffer),
            .in_memory => |f| {
                const to_read = @min(buffer.len, f.contents.len, f.chunk_size);
                @memcpy(buffer[0..to_read], f.contents[0..to_read]);
                self.data.in_memory.contents = f.contents[to_read..];
                return to_read;
            },
        };
    }
};

pub const InMemoryFile = struct {
    path: []const u8,
    contents: []const u8,
};

data: union(enum) {
    real: std.fs.Dir,
    in_memory: struct {
        allocator: std.mem.Allocator,
        chunk_size: usize = 4096,
        files: std.StringHashMapUnmanaged([]const u8) = .empty,
    },
},

pub fn real(dir: std.fs.Dir) Dir {
    return .{
        .data = .{ .real = dir },
    };
}

pub fn inMemory(
    allocator: std.mem.Allocator,
    files: []const InMemoryFile,
) error{ DuplicateFilePath, OutOfMemory }!Dir {
    var map: std.StringHashMapUnmanaged([]const u8) = .empty;
    errdefer map.deinit(allocator);

    try map.ensureTotalCapacity(allocator, @intCast(files.len));

    for (files) |file| {
        const entry = try map.getOrPutValue(allocator, file.path, file.contents);

        if (entry.value_ptr.ptr != file.contents.ptr) {
            return error.DuplicateFilePath;
        }
    }

    return .{
        .data = .{
            .in_memory = .{ .allocator = allocator, .files = map },
        },
    };
}

pub fn close(dir: *Dir) void {
    switch (dir.data) {
        .real => |*d| d.close(),
        .in_memory => |*d| {
            d.files.deinit(d.allocator);
        },
    }
}

pub fn openFile(self: *const Dir, sub_path: []const u8) std.fs.File.OpenError!File {
    return switch (self.data) {
        .real => |dir| {
            const file = try dir.openFile(sub_path, .{ .mode = .read_only });
            return .real(file);
        },
        .in_memory => |in_memory| {
            const contents = in_memory.files.get(sub_path) orelse return error.FileNotFound;
            return .inMemory(contents, in_memory.chunk_size);
        },
    };
}

pub fn fileExists(self: *const Dir, sub_path: []const u8) bool {
    return switch (self.data) {
        .real => |dir| {
            dir.access(sub_path, .{}) catch return false;
            return true;
        },
        .in_memory => |in_memory| in_memory.files.get(sub_path) != null,
    };
}
