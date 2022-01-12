const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const main_tests = b.addTest("src/tests.zig");
    main_tests.setBuildMode(mode);

    // test files are GPL licensed, so they are excluded from the distribution
    const download_tests = b.addSystemCommand(&.{ "python", "tools/download_tests.py", "src/tests" });
    main_tests.step.dependOn(&download_tests.step);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
