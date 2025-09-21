const ifctc = @import("root.zig");

const std = @import("std");
const config = @import("config");

const usage =
    \\{s}
    \\
    \\Checks that LINT.IfChange directives are respected.
    \\
    \\Usage:
    \\  git diff --unified | {s}
    \\
    \\Directives:
    \\
    \\  LINT.IfChange
    \\    Starts a block of code which must be followed by a matching
    \\    LINT.ThenChange(paths).
    \\
    \\  LINT.IfChange(label)
    \\    Starts a block of code which must be followed by a matching
    \\    LINT.ThenChange(paths), and which can be referred to using the specified
    \\    label.
    \\
    \\  LINT.ThenChange(paths)
    \\    Ends a block of code started by LINT.IfChange. If any of the lines in the
    \\    code block is modified, all of the specified files must also be modified.
    \\
    \\    Paths are a comma-separated list of paths to files which must be modified.
    \\    Relative paths are relative to the file being scanned, whereas absolute
    \\    paths (starting with `/` or `//`) are relative to the current working
    \\    directory when invoking this tool.
    \\
    \\    Paths can also end with `:<label>`, in which case the LINT.IfChange block
    \\    with the given label specifically must be modified. The file path can be
    \\    omitted when referring to a label in the same file.
;

const version_line = config.name ++ " " ++ config.version;

fn printUsage(argv0: []const u8) void {
    printOrExit(.stdout(), usage, .{ version_line, argv0 });
}

fn printVersion() void {
    printOrExit(.stdout(), version_line, .{});
}

const error_codes = struct {
    /// Some files had diagnostics.
    pub const diagnostics = 1;
    /// Bad input (e.g. invalid diff or arguments).
    pub const bad_input = 2;
    /// IO error (e.g. cannot read or write stdin/stdout).
    pub const io_error = 3;
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Validate command line arguments.
    blk: {
        var args = try std.process.argsWithAllocator(allocator);
        defer args.deinit();

        const argv0 = args.next() orelse break :blk;

        if (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                printUsage(argv0);
                std.process.exit(0);
            }
            if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                printVersion();
                std.process.exit(0);
            }
            printUsage(argv0);
            std.process.exit(error_codes.bad_input);
        }

        if (args.skip()) {
            printUsage(argv0);
            std.process.exit(error_codes.bad_input);
        }
    }

    // Prepare inputs.
    const cwd: std.fs.Dir = std.fs.cwd(); // Must not close `std.fs.cwd()`.
    var stdin: std.fs.File = .stdin();
    defer stdin.close();

    // Run logic.
    run(allocator, .real(cwd), &stdin) catch |err| switch (err) {
        error.InvalidDiff => {
            // An error message will have been printed.
            std.process.exit(error_codes.bad_input);
        },
        error.Diagnostics => {
            // Diagnostics will have been printed.
            std.process.exit(error_codes.diagnostics);
        },
        error.OutOfMemory => {
            printOrExit(.stderr(), "Out of memory", .{});
            std.process.exit(error_codes.io_error);
        },
        error.ReadFailed => {
            printOrExit(.stderr(), "Failed to read stdin", .{});
            std.process.exit(error_codes.io_error);
        },
        error.WriteFailed => {
            printOrExit(.stderr(), "Failed to write diagnostics to stdout", .{});
            std.process.exit(error_codes.io_error);
        },
    };
}

fn run(allocator: std.mem.Allocator, root_dir: ifctc.Analyzer.Dir, diff_file: *std.fs.File) !void {
    var rw_buffer: [4096]u8 = undefined;
    var diff_reader = diff_file.reader(&rw_buffer);

    // Analyze the diff given to `stdin`, using the current directory as the root.
    var analysis = try ifctc.Analysis.analyze(allocator, &diff_reader.interface, &root_dir);
    defer analysis.deinit();

    if (analysis.files.len == 0) {
        // No file with diagnostics.
        return;
    }

    // Print diagnostics.
    var stdout: std.fs.File = .stdout();
    defer stdout.close();
    var stdout_writer = stdout.writer(&rw_buffer);

    try analysis.printDiagnostics(&stdout_writer.interface);
    try stdout_writer.interface.flush();

    return error.Diagnostics;
}

/// Prints `fmt` to `dest` followed by a newline, exiting on error. Takes ownership of `dest`.
fn printOrExit(dest: std.fs.File, comptime fmt: []const u8, args: anytype) void {
    defer dest.close();

    var buffer: [1024]u8 = undefined;
    var writer = dest.writer(&buffer);
    defer writer.interface.flush() catch std.process.exit(error_codes.io_error);

    writer.interface.print(fmt ++ "\n", args) catch std.process.exit(error_codes.io_error);
}
