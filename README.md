# zig80 - z80 emulator

zig80 is a Z80 emulator written in Zig.

## Features

- Cycle counting
- Support for undocumented behavior

## Example

```zig
const std = @import("std");
const z80 = @import("zig80");

// object to implement read callback
const Computer = struct {
    mem: []const u8,

    fn read(self: *Computer, addr: u16) u8 {
        return self.mem[addr];
    }

    fn interface(self: *Computer) z80.Interface {
        return z80.Interface.init(self, .{ .read = read });
    }
};

// calculate the 20th fibonacci number
const N = 20;

pub fn main() void {
    // program to calculate fibonacci number
    // b = index of fibonacci number to calculate
    // hl = result
    const program = [_]u8{
        0x21, 0x00, 0x00, //   ld   hl,0000h
        0x11, 0x01, 0x00, //   ld   de,0001h
                          // loop:
        0x19,             //   add  hl,de
        0xeb,             //   ex   de,hl
        0x10, 0xfc,       //   dnjz loop
        0x76,             //   halt
    };
    var computer = Computer{ .mem = &program };
    // initialize CPU with interface attached
    var cpu = z80.CPU{ .interface = computer.interface() };
    // load input parameter for program
    cpu.setB(N);
    // run program
    while (!cpu.halted) {
        cpu.step();
    }
    // get the result, check that it is correct
    std.debug.assert(cpu.getHL() == 6765);
}
```

## License

This library is provided with the MIT license. The tests use GPL-licensed
programs which are not bundled with this library but will be downloaded as
needed to run tests.

## Acknowledgements

The following resources had a significant impact on the design of the emulator:

- <https://github.com/superzazu/z80>
- <http://z80.info/>
- <https://gist.github.com/drhelius/8497817>
- <https://clrhome.org/table/>
