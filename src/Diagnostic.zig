//! An error encountered during analysis.
const Diagnostic = @This();
const File = @import("root.zig").Analysis.File;

const std = @import("std");

pub const Details = union(enum) {
    // IO errors.
    cannot_open: std.fs.File.OpenError,
    cannot_read: std.fs.File.ReadError,

    // Parse errors.
    invalid_directive,
    if_change_follows_if_change: struct {
        if_change_line: u32,
    },
    then_change_follows_then_change: struct {
        then_change_line: u32,
    },
    then_change_without_if_change,

    // Invalid directive contents.
    self_file,
    self_label: struct {
        label: []const u8,
    },
    duplicate_label: struct {
        label: []const u8,
        previous_line: u32,
    },
    invalid_path: []const u8,
    path_escapes_root: []const u8,
    file_not_found: []const u8,

    // Temporary error.
    label_status_not_yet_available: struct {
        file_id: u32,
        label: []const u8,
    },

    // Missing changes.
    file_not_modified: []const u8,
    file_deleted: u32,
    file_renamed_but_not_modified: u32,
    label_does_not_exist: []const u8,
    label_not_modified: []const u8,
    binary_file_cannot_have_labels: u32,
};

/// The line where the diagnostic begins, or 0 if the whole file is concerned.
line: u32,

details: Details,

pub fn copy(self: *const Diagnostic, allocator: std.mem.Allocator) error{OutOfMemory}!Diagnostic {
    return .{
        .line = self.line,
        .details = try self.copyDetails(allocator),
    };
}

pub fn print(
    self: *const Diagnostic,
    writer: *std.Io.Writer,
    path: []const u8,
    files: []const File,
) error{WriteFailed}!void {
    try writer.writeAll(path);
    if (self.line != 0) {
        try writer.print(":{d}: ", .{self.line});
    } else {
        try writer.writeAll(": ");
    }
    try self.printDetails(writer, files);
}

fn copyDetails(self: *const Diagnostic, allocator: std.mem.Allocator) error{OutOfMemory}!Details {
    var details = self.details;

    switch (details) {
        .cannot_open,
        .cannot_read,
        .invalid_directive,
        .if_change_follows_if_change,
        .then_change_follows_then_change,
        .then_change_without_if_change,
        .self_file,
        .file_deleted,
        .file_renamed_but_not_modified,
        .binary_file_cannot_have_labels,
        => {},

        inline .self_label,
        .duplicate_label,
        .label_status_not_yet_available,
        => |*d| {
            d.label = try allocator.dupe(u8, d.label);
        },

        inline .invalid_path,
        .path_escapes_root,
        .file_not_found,
        .file_not_modified,
        .label_does_not_exist,
        .label_not_modified,
        => |*path| {
            path.* = try allocator.dupe(u8, path.*);
        },
    }

    return details;
}

fn printDetails(
    self: *const Diagnostic,
    writer: *std.Io.Writer,
    files: []const File,
) error{WriteFailed}!void {
    switch (self.details) {
        .label_status_not_yet_available => {
            @panic("caller must ignore or update .label_status_not_yet_available");
        },

        .cannot_open => |err| {
            try writer.print("cannot open file: {}", .{err});
        },
        .cannot_read => |err| {
            try writer.print("cannot read file: {}", .{err});
        },

        .invalid_directive => {
            try writer.print("invalid directive", .{});
        },
        .if_change_follows_if_change => |d| {
            try writer.print("LINT.IfChange follows another LINT.IfChange on line {d}", .{d.if_change_line});
        },
        .then_change_follows_then_change => |d| {
            try writer.print("LINT.ThenChange follows another LINT.ThenChange on line {d}", .{d.then_change_line});
        },
        .then_change_without_if_change => {
            try writer.print("LINT.ThenChange without preceding LINT.IfChange", .{});
        },

        .self_file => {
            try writer.print("LINT.ThenChange requires a change in the same file", .{});
        },
        .self_label => |d| {
            try writer.print("label \"{s}\" requires a change to its own block of code", .{d.label});
        },
        .duplicate_label => |d| {
            try writer.print("label \"{s}\" already defined on line {d}", .{ d.label, d.previous_line });
        },
        .invalid_path => |path| {
            try writer.print("invalid path: {s}", .{path});
        },
        .path_escapes_root => |path| {
            try writer.print("path points outside root directory: {s}", .{path});
        },
        .file_not_found => |path| {
            try writer.print("file not found: {s}", .{path});
        },

        .file_not_modified => |path| {
            try writer.print("file was not modified: {s}", .{path});
        },
        .file_deleted => |file_id| {
            try writer.print("file was deleted: {s}", .{files[file_id].path});
        },
        .file_renamed_but_not_modified => |file_id| {
            try writer.print("file was renamed but not modified: {s}", .{files[file_id].path});
        },
        .label_does_not_exist => |label| {
            try writer.print("label \"{s}\" does not exist", .{label});
        },
        .label_not_modified => |label| {
            try writer.print("code block with label \"{s}\" not modified", .{label});
        },
        .binary_file_cannot_have_labels => |file_id| {
            try writer.print("binary file cannot have labels: {s}", .{files[file_id].path});
        },
    }
}
