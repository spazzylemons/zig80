pub const CPU = @import("CPU.zig");

pub const Flags = struct {
    pub const C: u8 = 1 << 0;
    pub const N: u8 = 1 << 1;
    pub const PV: u8 = 1 << 2;
    pub const X: u8 = 1 << 3;
    pub const H: u8 = 1 << 4;
    pub const Y: u8 = 1 << 5;
    pub const Z: u8 = 1 << 6;
    pub const S: u8 = 1 << 7;
};

pub const Interface = @import("Interface.zig");

pub const InterruptMode = enum { Mode0, Mode1, Mode2 };
