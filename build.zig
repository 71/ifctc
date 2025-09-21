const std = @import("std");
const build_config: struct {
    name: @Type(.enum_literal),
    version: []const u8,
    fingerprint: u64,
    minimum_zig_version: []const u8,
    dependencies: struct {},
    paths: []const []const u8,
} = @import("build.zig.zon");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("ifctc", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    const exe = b.addExecutable(.{
        .name = "ifctc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = false,
        }),
    });

    b.installArtifact(exe);

    const options = b.addOptions();
    options.addOption([]const u8, "version", build_config.version);
    options.addOption([]const u8, "name", @tagName(build_config.name));
    exe.root_module.addOptions("config", options);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // Creates an executable that will run `test` blocks from the provided module.
    const mod_tests = b.addTest(.{
        .root_module = mod,
    });

    const run_mod_tests = b.addRunArtifact(mod_tests);

    // Creates an executable that will run `test` blocks from the executable's
    // root module.
    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });
    const run_exe_tests = b.addRunArtifact(exe_tests);

    // Also run example tests.
    const run_example_tests = b.addSystemCommand(&[1][]const u8{
        b.pathFromRoot("example/test.sh"),
    });

    // A top level step for running all tests.
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);
    test_step.dependOn(&run_exe_tests.step);
    test_step.dependOn(&run_example_tests.step);

    // A top level step for building for multiple architectures.
    const build_all_step = b.step("all", "Build for multiple architectures");
    const build_all_targets: []const std.Target.Query = &.{
        .{ .cpu_arch = .aarch64, .os_tag = .macos },
        .{ .cpu_arch = .aarch64, .os_tag = .linux },
        .{ .cpu_arch = .x86_64, .os_tag = .linux },
        .{ .cpu_arch = .x86_64, .os_tag = .windows },
    };

    for (build_all_targets) |build_all_target| {
        const target_exe = b.addExecutable(.{
            .name = try std.mem.concat(
                b.allocator,
                u8,
                &[_][]const u8{ "ifctc-", try build_all_target.zigTriple(b.allocator) },
            ),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = b.resolveTargetQuery(build_all_target),
                .optimize = .ReleaseSafe,
                .link_libc = false,
                .strip = true,
            }),
        });
        target_exe.root_module.addOptions("config", options);

        const target_output = b.addInstallArtifact(target_exe, .{});

        build_all_step.dependOn(&target_output.step);
    }
}
