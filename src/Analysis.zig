const Analysis = @This();

test {
    _ = @import("Analysis_test.zig");
}

const Analyzer = @import("Analyzer.zig");
const ChangeSet = @import("ChangeSet.zig");
const std = @import("std");

pub const File = struct {
    path: []const u8,
    diagnostics: []const Analyzer.Diagnostic,
};

arena: std.heap.ArenaAllocator,
files: []File,

pub fn deinit(self: *Analysis) void {
    self.arena.deinit();
}

pub fn printDiagnostics(self: *const Analysis, writer: *std.Io.Writer) error{WriteFailed}!void {
    for (self.files) |*file| {
        for (file.diagnostics) |*diag| {
            try diag.print(writer, file.path, self.files);
            try writer.writeByte('\n');
        }
    }
}

pub const Error = ChangeSet.DiffParser.Error || error{ReadFailed};

/// Parses `diff_reader` as a unified diff, then scans the given `directory` to see if files
/// changed in the diff have corresponding changes in the source code.
pub fn analyze(
    allocator: std.mem.Allocator,
    diff_reader: *std.Io.Reader,
    directory: *const Analyzer.Dir,
) Error!Analysis {
    // Use arena allocation since the program is short-lived.
    var arena: std.heap.ArenaAllocator = .init(allocator);
    errdefer arena.deinit();

    // Load changed ranges.
    var changes: ChangeSet = .init(arena.allocator());
    {
        var scratch_inner = std.heap.stackFallback(4096, allocator);
        var scratch: std.heap.ArenaAllocator = .init(scratch_inner.get());
        defer scratch.deinit();

        try changes.parseDiff(scratch.allocator(), diff_reader);
    }
    if (changes.files.items.len == 0) {
        return .{
            .arena = arena,
            .files = &.{},
        };
    }

    // Prepare worker threads.
    const cpu_count = @max(std.Thread.getCpuCount() catch 4, 1);
    const thread_count = @min(cpu_count, changes.files.items.len);
    const worker_count = thread_count - 1;

    var main_thread: Worker = try .init(allocator);
    var workers = try arena.allocator().alloc(Worker, worker_count);
    defer arena.allocator().free(workers);

    for (workers) |*worker| {
        // Make sure to use `allocator`, not `arena.allocator()`, for all worker threads.
        worker.* = try .init(allocator);
    }

    // Prepare analyzer.
    var analyzer: Analyzer = try .init(allocator, &changes, directory);
    defer analyzer.deinit();

    // Spawn worker threads.
    for (workers, 0..) |*worker, i| {
        worker.thread = std.Thread.spawn(
            .{ .allocator = allocator },
            Worker.analyzeAll,
            .{ worker, &analyzer },
        ) catch {
            // If we can't create a thread, fail gracefully by running the analysis in the main
            // thread and the threads we managed to spawn so far.
            for (workers[i + 1 ..]) |*not_started_worker| {
                not_started_worker.* = try .init(allocator);
            }
            break;
        };
    }

    // Keep working until all files have been processed.
    {
        defer for (workers) |*worker| if (worker.thread) |thread| thread.join();

        main_thread.analyzeAll(&analyzer);

        if (main_thread.err) |e| return e;
    }

    // Check if any of the workers failed.
    for (workers) |*worker| {
        if (worker.err) |e| {
            return e;
        }
    }

    // Collect files into an `Analysis`.
    var files = try arena.allocator().alloc(Analysis.File, analyzer.files.len);
    var files_len: usize = 0;

    errdefer {
        for (files[0..files_len]) |*file| {
            arena.allocator().free(file.diagnostics);
        }
        arena.allocator().free(files);
    }

    for (analyzer.files) |*analyzed_file| {
        const diagnostics_len = analyzed_file.fixupDiagnostics(analyzer.files);
        const diagnostics = try analyzed_file.copyDiagnostics(arena.allocator(), diagnostics_len);
        if (diagnostics.len == 0) continue;

        // Paths in `changes` are allocated in `arena`.
        files[files_len] = .{
            .path = analyzed_file.change.path,
            .diagnostics = diagnostics,
        };
        files_len += 1;
    }

    return .{ .arena = arena, .files = files[0..files_len] };
}

/// State associated with a worker thread, i.e. its thread and any error it encountered.
const Worker = struct {
    const stack_size = 4096;

    worker_allocator: std.heap.StackFallbackAllocator(stack_size),
    per_file_allocator: std.heap.StackFallbackAllocator(stack_size),

    worker_arena: std.heap.ArenaAllocator,
    per_file_arena: std.heap.ArenaAllocator,

    /// Null if the thread couldn't be spawned.
    thread: ?std.Thread = null,
    err: ?error{OutOfMemory} = null,

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!Worker {
        return .{
            .worker_allocator = std.heap.stackFallback(stack_size, allocator),
            .per_file_allocator = std.heap.stackFallback(stack_size, allocator),
            .worker_arena = undefined,
            .per_file_arena = undefined,
        };
    }

    /// The function executed by each worker thread.
    pub fn analyzeAll(self: *Worker, analyzer: *const Analyzer) void {
        self.worker_arena = .init(self.worker_allocator.get());
        self.per_file_arena = .init(self.per_file_allocator.get());

        analyzer.analyzeAll(&self.worker_arena, &self.per_file_arena) catch |e| {
            self.err = e;
        };
    }
};
