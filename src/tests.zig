//! tests based on test suite setup in https://github.com/superzazu/z80,
//! with some tweaks

const std = @import("std");
const z80 = @import("zig80.zig");

inline fn stderr() std.fs.File.Writer {
    return std.io.getStdErr().writer();
}

const Tester = struct {
    cpu: z80.CPU,
    failed: bool = false,
    memory: [0x10000]u8 = [_]u8 { 0 } ** 0x10000,

    fn read(self: *Tester, addr: u16) u8 {
        return self.memory[addr];
    }

    fn write(self: *Tester, addr: u16, value: u8) void {
        self.memory[addr] = value;
    }

    fn out(self: *Tester, port: u16, value: u8) void {
        _ = port;
        _ = value;
        switch (self.cpu.getC()) {
            // e = char to print
            2 => {
                const char = self.cpu.getE();
                stderr().writeByte(char) catch {};
            },
            // de = '$'-terminated string
            9 => {
                const addr = self.cpu.getDE();
                const str = std.mem.sliceTo(self.memory[addr..], '$');
                // if string contains "ERROR", then a test failed
                if (std.mem.indexOf(u8, str, "ERROR") != null) {
                    self.failed = true;
                }
                stderr().writeAll(str) catch {};
            },
            else => {},
        }
    }

    fn run(rom: []const u8, expected_cycles: u64) !void {
        // tester object
        var self = Tester{ .cpu = undefined };
        // store COM file starting at 100h
        std.mem.copy(u8, self.memory[0x100..], rom);
        // cp/m warm boot function:
        self.memory[0x0000] = 0x76; // halt
        // cp/m bdos function:
        self.memory[0x0005] = 0xd3; // out  (0),a ; trigger i/o callback
        self.memory[0x0006] = 0x00; //
        self.memory[0x0007] = 0xc9; // ret
        // z80 cpu
        self.cpu = .{
            .interface = z80.Interface.init(&self, .{
                .read = read,
                .write = write,
                .out = out,
            }),
            .pc = 0x100,
        };
        // check for cycle accuracy
        var actual_cycles: u64 = 0;
        while (!self.cpu.halted) {
            // step cpu, add to total cycle count
            self.cpu.step();
            actual_cycles += self.cpu.cycles;
            self.cpu.cycles = 0;
        }
        // if the exerciser reported at least one error, then fail
        if (self.failed) {
            std.debug.print("one or more tests failed\n", .{});
            return error.TestFailed;
        }
        // check for cycle accuracy
        try std.testing.expectEqual(expected_cycles, actual_cycles);
    }
};

test "documented instructions" {
    try Tester.run(@embedFile("tests/zexdoc.com"), 46734978642);
}

test "all instructions" {
    try Tester.run(@embedFile("tests/zexall.com"), 46734978642);
}
