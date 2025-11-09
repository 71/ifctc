const Analysis = @import("Analysis.zig");
const Diagnostic = @import("Diagnostic.zig");
const Dir = @import("Dir.zig");
const std = @import("std");

// Hint: use `tools/mkdiff.sh zig <file-name> <old> <new>` to generate diffs for tests.

test Analysis {
    // This test comes from the README example.

    // LINT.IfChange(example)
    try expectDiagnostics(
        &[_]Analysis.File{},
        \\diff --git a/constants.py b/constants.py
        \\--- a/constants.py
        \\+++ b/constants.py
        \\@@ -1 +1,3 @@
        \\+# LINT.IfChange
        \\ MIN_VERSION = "0.2.0"
        \\+# LINT.ThenChange(constants.rs)
        \\diff --git a/constants.rs b/constants.rs
        \\--- a/constants.rs
        \\+++ b/constants.rs
        \\@@ -1 +1,3 @@
        \\-const MIN_VERSION: &str = "0.2.0"
        \\+// LINT.IfChange
        \\+const MIN_VERSION: &str = "0.2.0";
        \\+// LINT.ThenChange(constants.py)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "constants.py",
                .contents =
                \\# LINT.IfChange
                \\MIN_VERSION = "0.2.0"
                \\# LINT.ThenChange(constants.rs)
                ,
            },
            .{
                .path = "constants.rs",
                .contents =
                \\// LINT.IfChange
                \\const MIN_VERSION: &str = "0.2.0";
                \\// LINT.ThenChange(constants.py)
                ,
            },
        },
    );

    try expectDiagnostics(
        &[_]Analysis.File{
            .{
                .path = "constants.rs",
                .diagnostics = &[_]Diagnostic{
                    .{
                        .line = 3,
                        .details = .{ .file_not_modified = "constants.py" },
                    },
                },
            },
        },
        \\diff --git a/constants.rs b/constants.rs
        \\--- a/constants.rs
        \\+++ b/constants.rs
        \\@@ -1,3 +1,3 @@
        \\ // LINT.IfChange
        \\-const MIN_VERSION: &str = "0.2.0";
        \\+const MIN_VERSION: &str = "0.3.0";
        \\ // LINT.ThenChange(constants.py)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "constants.py",
                .contents =
                \\# LINT.IfChange
                \\MIN_VERSION = "0.2.0"
                \\# LINT.ThenChange(constants.rs)
                ,
            },
            .{
                .path = "constants.rs",
                .contents =
                \\// LINT.IfChange
                \\const MIN_VERSION: &str = "0.3.0";
                \\// LINT.ThenChange(constants.py)
                ,
            },
        },
    );

    try expectDiagnostics(
        &[_]Analysis.File{},
        \\diff --git a/constants.py b/constants.py
        \\--- a/constants.py
        \\+++ b/constants.py
        \\@@ -1,3 +1,3 @@
        \\ # LINT.IfChange
        \\-MIN_VERSION = "0.2.0"
        \\+MIN_VERSION = "0.3.0"
        \\ # LINT.ThenChange(constants.rs)
        \\diff --git a/constants.rs b/constants.rs
        \\--- a/constants.rs
        \\+++ b/constants.rs
        \\@@ -1,3 +1,3 @@
        \\ // LINT.IfChange
        \\-const MIN_VERSION: &str = "0.2.0";
        \\+const MIN_VERSION: &str = "0.3.0";
        \\ // LINT.ThenChange(constants.py)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "constants.py",
                .contents =
                \\# LINT.IfChange
                \\MIN_VERSION = "0.3.0"
                \\# LINT.ThenChange(constants.rs)
                ,
            },
            .{
                .path = "constants.rs",
                .contents =
                \\// LINT.IfChange
                \\const MIN_VERSION: &str = "0.3.0";
                \\// LINT.ThenChange(constants.py)
                ,
            },
        },
    );

    // LINT.ThenChange(/README.md:example)
}

test "no directive" {
    try expectDiagnostics(
        &[_]Analysis.File{},
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1,2 +1,2 @@
        \\ a
        \\-b1
        \\+b2
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "foo",
                .contents =
                \\a
                \\b2
                ,
            },
        },
    );
}

test "self label" {
    try expectDiagnostics(
        &[_]Analysis.File{
            .{
                .path = "foo",
                .diagnostics = &[_]Diagnostic{
                    .{
                        .line = 3,
                        .details = .{ .self_label = .{ .label = "a" } },
                    },
                },
            },
        },
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1,3 +1,3 @@
        \\ # LINT.IfChange(a)
        \\-a
        \\+b
        \\ # LINT.ThenChange(:a)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "foo",
                .contents =
                \\# LINT.IfChange(a)
                \\b
                \\# LINT.ThenChange(:a)
                ,
            },
        },
    );
}

test "self file" {
    try expectDiagnostics(
        &[_]Analysis.File{
            .{
                .path = "foo",
                .diagnostics = &[_]Diagnostic{
                    .{
                        .line = 3,
                        .details = .self_file,
                    },
                },
            },
        },
        \\diff --git a/foo b/foo
        \\--- a/foo
        \\+++ b/foo
        \\@@ -1,3 +1,3 @@
        \\ # LINT.IfChange
        \\-a
        \\+b
        \\ # LINT.ThenChange(foo)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "foo",
                .contents =
                \\# LINT.IfChange
                \\b
                \\# LINT.ThenChange(foo)
                ,
            },
        },
    );
}

test "absolute path" {
    try expectDiagnostics(
        &[_]Analysis.File{
            .{
                .path = "foo/bar",
                .diagnostics = &[_]Diagnostic{
                    .{
                        .line = 3,
                        .details = .{ .file_not_modified = "baz/quux" },
                    },
                },
            },
        },
        \\diff --git a/foo/bar b/foo/bar
        \\--- a/foo/bar
        \\+++ b/foo/bar
        \\@@ -1,3 +1,3 @@
        \\ # LINT.IfChange
        \\-a
        \\+b
        \\ # LINT.ThenChange(//baz/quux)
    ,
        &[_]Dir.InMemoryFile{
            .{
                .path = "foo/bar",
                .contents =
                \\# LINT.IfChange
                \\b
                \\# LINT.ThenChange(//baz/quux)
                ,
            },
            .{
                .path = "baz/quux",
                .contents =
                \\
                ,
            },
        },
    );
}

test "deleted file" {
    try expectDiagnostics(
        &[_]Analysis.File{},
        \\diff --git a/foo b/foo
        \\deleted file mode 100644
        \\--- a/foo
        \\+++ /dev/null
        \\@@ -1 +0,0 @@
        \\-a
    ,
        &[_]Dir.InMemoryFile{},
    );
}

// -------------------------------------------------------------------------------------------------
// MARK: Helpers

fn expectDiagnostics(
    expected: []const Analysis.File,
    diff: []const u8,
    files: []const Dir.InMemoryFile,
) !void {
    var analysis = try analyze(diff, files);
    defer analysis.deinit();

    try std.testing.expectEqualDeep(expected, analysis.files);
}

fn analyze(
    diff: []const u8,
    files: []const Dir.InMemoryFile,
) !Analysis {
    var diff_reader: std.Io.Reader = .fixed(diff);
    var directory: Dir = try .inMemory(std.testing.allocator, files);
    defer directory.close();
    return .analyze(std.testing.allocator, &diff_reader, &directory);
}
