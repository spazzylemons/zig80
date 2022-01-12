//! The Z80 CPU.

const CPU = @This();

const pkg = @import("zig80.zig");
const Flags         = pkg.Flags;
const Interface     = pkg.Interface;
const InterruptMode = pkg.InterruptMode;

/// The cycle counts for the main instructions, excluding cycles added by conditionally branching.
const CYCLE_COUNTS = [256]u8 {
    4, 10,  7,  6,  4,  4,  7,  4,  4, 11,  7,  6,  4,  4,  7,  4,
    8, 10,  7,  6,  4,  4,  7,  4, 12, 11,  7,  6,  4,  4,  7,  4,
    7, 10, 16,  6,  4,  4,  7,  4,  7, 11, 16,  6,  4,  4,  7,  4,
    7, 10, 13,  6, 11, 11, 10,  4,  7, 11, 13,  6,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    7,  7,  7,  7,  7,  7,  4,  7,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    4,  4,  4,  4,  4,  4,  7,  4,  4,  4,  4,  4,  4,  4,  7,  4,
    5, 10, 10, 10, 10, 11,  7, 11,  5, 10, 10,  4, 10, 17,  7, 11,
    5, 10, 10, 11, 10, 11,  7, 11,  5,  4, 10, 11, 10,  4,  7, 11,
    5, 10, 10, 19, 10, 11,  7, 11,  5,  4, 10,  4, 10,  4,  7, 11,
    5, 10, 10,  4, 10, 11,  7, 11,  5,  6, 10,  4, 10,  4,  7, 11,
};

/// The cycle counts for the extended instructions, minus four for the prefix fetch.
const ED_CYCLE_COUNTS = [256]u8 {
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     8,  8, 11, 16,  4, 10,  4,  5,  8,  8, 11, 16,  4, 10,  4,  5,
     8,  8, 11, 16,  4, 10,  4,  5,  8,  8, 11, 16,  4, 10,  4,  5,
     8,  8, 11, 16,  4, 10,  4, 14,  8,  8, 11, 16,  4, 10,  4, 14,
     8,  8, 11, 16,  4, 10,  4,  4,  8,  8, 11, 16,  4, 10,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
    12, 12, 12, 12,  4,  4,  4,  4, 12, 12, 12, 12,  4,  4,  4,  4,
    12, 12, 12, 12,  4,  4,  4,  4, 12, 12, 12, 12,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
};

/// The cycle counts for the indexed instructions, minus four for the prefix fetch.
const IZ_CYCLE_COUNTS = [256]u8 {
     0,  0,  0,  0,  0,  0,  0,  0,  0, 11,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0, 11,  0,  0,  0,  0,  0,  0,
     0, 10, 16,  6,  4,  4,  7,  0,  0, 11, 16,  6,  4,  4,  7,  0,
     0,  0,  0,  0, 19, 19, 15,  0,  0, 11,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     4,  4,  4,  4,  4,  4, 15,  4,  4,  4,  4,  4,  4,  4, 15,  4,
    15, 15, 15, 15, 15, 15,  0, 15,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  4,  4, 15,  0,  0,  0,  0,  0,  4,  4, 15,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  4,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0, 10,  0, 19,  0, 11,  0,  0,  0,  4,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  6,  0,  0,  0,  0,  0,  0,
};

/// A register pair, accessible as a single word or a pair of bytes.
const Pair = packed union {
    w: u16,
    b: switch (@import("builtin").cpu.arch.endian()) {
        .Big => packed struct { h: u8, l: u8 },
        .Little => packed struct { l: u8, h: u8 },
    },
};

/// Create a word from two bytes.
inline fn word(h: u8, l: u8) u16 {
    return (@as(u16, h) << 8) | l;
}

/// Get the high byte of a word.
inline fn hiByte(w: u16) u8 {
    return @intCast(u8, w >> 8);
}

/// Get the low byte of a word.
inline fn loByte(w: u16) u8 {
    return @truncate(u8, w);
}

interface: Interface,

af: Pair = .{ .w = 0xffff },
bc: Pair = .{ .w = 0 },
de: Pair = .{ .w = 0 },
hl: Pair = .{ .w = 0 },

pc: u16 = 0,
sp: u16 = 0,

ix: Pair = .{ .w = 0 },
iy: Pair = .{ .w = 0 },

/// internal "memptr" register
wz: u16 = 0,

af2: Pair = .{ .w = 0 },
bc2: Pair = .{ .w = 0 },
de2: Pair = .{ .w = 0 },
hl2: Pair = .{ .w = 0 },

i: u8 = 0,
r: u8 = 0,
im: InterruptMode = .Mode0,

iff1: bool = false,
iff2: bool = false,
int_delay: bool = false,
int_read: bool = false,
ld_ir: bool = false,
halted: bool = false,

index_type: enum { HL, IX, IY } = .HL,

cycles: u32 = 0,

inline fn readByte(self: *const CPU, addr: u16) u8 {
    return self.interface.read(addr);
}

inline fn readWord(self: *const CPU, addr: u16) u16 {
    const lo = self.readByte(addr);
    const hi = self.readByte(addr +% 1);
    return word(hi, lo);
}

fn readWordMemPtr(self: *CPU) u16 {
    self.wz = self.fetchWord() +% 1;
    return self.readWord(self.wz -% 1);
}

inline fn writeByte(self: *const CPU, addr: u16, value: u8) void {
    return self.interface.write(addr, value);
}

inline fn writeWord(self: *const CPU, addr: u16, value: u16) void {
    self.writeByte(addr, loByte(value));
    self.writeByte(addr +% 1, hiByte(value));
}

fn writeWordMemPtr(self: *CPU, value: u16) void {
    self.wz = self.fetchWord() +% 1;
    self.writeWord(self.wz -% 1, value);
}

inline fn in(self: *const CPU, port: u16) u8 {
    return self.interface.in(port);
}

inline fn out(self: *const CPU, port: u16, value: u8) void {
    return self.interface.out(port, value);
}

fn fetchByte(self: *CPU) u8 {
    if (self.int_read) {
        return self.interface.irq();
    } else {
        self.pc +%= 1;
        return self.readByte(self.pc -% 1);
    }
}

inline fn fetchWord(self: *CPU) u16 {
    const lo = self.fetchByte();
    const hi = self.fetchByte();
    return word(hi, lo);
}

fn signedByte(value: u8) u16 {
    return @bitCast(u16, @as(i16, @bitCast(i8, value)));
}

inline fn izd(self: *CPU, iz: u16) u16 {
    self.wz = iz +% signedByte(self.fetchByte());
    return self.wz;
}

inline fn readIZD(self: *CPU, iz: u16) u8 {
    return self.readByte(self.izd(iz));
}

inline fn refresh(self: *CPU) void {
    self.r = ((self.r +% 1) & 0x7f) | (self.r & 0x80);
}

inline fn push(self: *CPU, value: u16) void {
    self.sp -%= 2;
    return self.writeWord(self.sp, value);
}

inline fn pop(self: *CPU) u16 {
    self.sp +%= 2;
    return self.readWord(self.sp -% 2);
}

inline fn getCarry(self: *const CPU) bool {
    return @bitCast(bool, @truncate(u1, self.getF()));
}

inline fn checkFlag(self: *const CPU, flag: u8) bool {
    return self.getF() & flag != 0;
}

inline fn andFlags(self: *CPU, flags: u8) void {
    self.setF(self.getF() & flags);
}

inline fn orFlags(self: *CPU, flags: u8) void {
    self.setF(self.getF() | flags);
}

inline fn flipFlags(self: *CPU, flags: u8) void {
    self.setF(self.getF() ^ flags);
}

// register getters
pub inline fn getA(self: *const CPU) u8 { return self.af.b.h; }
pub inline fn getF(self: *const CPU) u8 { return self.af.b.l; }
pub inline fn getB(self: *const CPU) u8 { return self.bc.b.h; }
pub inline fn getC(self: *const CPU) u8 { return self.bc.b.l; }
pub inline fn getD(self: *const CPU) u8 { return self.de.b.h; }
pub inline fn getE(self: *const CPU) u8 { return self.de.b.l; }
pub inline fn getH(self: *const CPU) u8 { return self.hl.b.h; }
pub inline fn getL(self: *const CPU) u8 { return self.hl.b.l; }
// register setters
pub inline fn setA(self: *CPU, value: u8) void { self.af.b.h = value; }
pub inline fn setF(self: *CPU, value: u8) void { self.af.b.l = value; }
pub inline fn setB(self: *CPU, value: u8) void { self.bc.b.h = value; }
pub inline fn setC(self: *CPU, value: u8) void { self.bc.b.l = value; }
pub inline fn setD(self: *CPU, value: u8) void { self.de.b.h = value; }
pub inline fn setE(self: *CPU, value: u8) void { self.de.b.l = value; }
pub inline fn setH(self: *CPU, value: u8) void { self.hl.b.h = value; }
pub inline fn setL(self: *CPU, value: u8) void { self.hl.b.l = value; }
// register pair getters
pub inline fn getAF(self: *const CPU) u16 { return self.af.w; }
pub inline fn getBC(self: *const CPU) u16 { return self.bc.w; }
pub inline fn getDE(self: *const CPU) u16 { return self.de.w; }
pub inline fn getHL(self: *const CPU) u16 { return self.hl.w; }
// register pair setters
pub inline fn setAF(self: *CPU, value: u16) void { self.af.w = value; }
pub inline fn setBC(self: *CPU, value: u16) void { self.bc.w = value; }
pub inline fn setDE(self: *CPU, value: u16) void { self.de.w = value; }
pub inline fn setHL(self: *CPU, value: u16) void { self.hl.w = value; }
// register pair increment
pub inline fn incAF(self: *CPU) void { self.af.w +%= 1; }
pub inline fn incBC(self: *CPU) void { self.bc.w +%= 1; }
pub inline fn incDE(self: *CPU) void { self.de.w +%= 1; }
pub inline fn incHL(self: *CPU) void { self.hl.w +%= 1; }
// regiser pair decrement
pub inline fn decAF(self: *CPU) void { self.af.w -%= 1; }
pub inline fn decBC(self: *CPU) void { self.bc.w -%= 1; }
pub inline fn decDE(self: *CPU) void { self.de.w -%= 1; }
pub inline fn decHL(self: *CPU) void { self.hl.w -%= 1; }

inline fn addCycles(self: *CPU, amount: u32) void {
    self.cycles +%= amount;
}

fn readAWithMemPtr(self: *CPU, addr: u16) void {
    self.setA(self.readByte(addr));
    self.wz = addr +% 1;
}

fn writeAWithMemPtr(self: *CPU, addr: u16) void {
    self.writeByte(addr, self.getA());
    self.wz = word(self.getA(), loByte(addr) +% 1);
}

inline fn readPairWithMemPtr(self: *CPU) u16 {
    self.wz = self.fetchWord() +% 1;
    return self.readWord(self.wz -% 1);
}

inline fn writePairWithMemPtr(self: *CPU, pair: u16) void {
    self.wz = self.fetchWord() +% 1;
    return self.writeWord(self.wz -% 1, pair);
}

fn readHL(self: *const CPU) u8 {
    return self.readByte(self.getHL());
}

fn writeHL(self: *const CPU, value: u8) void {
    self.writeByte(self.getHL(), value);
}

fn addBytes(self: *CPU, v1: u8, v2: u8, carry: bool) u8 {
    const long = @as(u16, v1) + v2 + @boolToInt(carry);
    const result = loByte(long);
    const carry_flag = hiByte(long);
    const carry_mask = v1 ^ v2 ^ result;
    self.setF(0);
    self.orFlags(result & (Flags.S | Flags.X | Flags.Y)); // sign and undoc
    if (result == 0) self.orFlags(Flags.Z);               // zero
    self.orFlags(carry_mask & Flags.H);                   // half-carry
    self.orFlags(((carry_mask >> 7) ^ carry_flag) << 2);  // overflow
    self.orFlags(carry_flag);                             // carry
    return result;
}

fn subBytes(self: *CPU, v1: u8, v2: u8, carry: bool) u8 {
    const result = self.addBytes(v1, ~v2, !carry);
    self.flipFlags(Flags.C | Flags.H | Flags.N);
    return result;
}

fn addWords(self: *CPU, v1: u16, v2: u16, carry: bool) u16 {
    const lo = self.addBytes(loByte(v1), loByte(v2), carry);
    const hi = self.addBytes(hiByte(v1), hiByte(v2), self.getCarry());
    const result = word(hi, lo);

    self.andFlags(~Flags.Z);
    if (result == 0) self.orFlags(Flags.Z);
    self.wz = v1 +% 1;

    return result;
}

fn subWords(self: *CPU, v1: u16, v2: u16, carry: bool) u16 {
    const lo = self.subBytes(loByte(v1), loByte(v2), carry);
    const hi = self.subBytes(hiByte(v1), hiByte(v2), self.getCarry());
    const result = word(hi, lo);

    self.andFlags(~Flags.Z);
    if (result == 0) self.orFlags(Flags.Z);
    self.wz = v1 +% 1;

    return result;
}

fn addA(self: *CPU, value: u8) void {
    self.setA(self.addBytes(self.getA(), value, false));
}

fn adcA(self: *CPU, value: u8) void {
    self.setA(self.addBytes(self.getA(), value, self.getCarry()));
}

fn subA(self: *CPU, value: u8) void {
    self.setA(self.subBytes(self.getA(), value, false));
}

fn sbcA(self: *CPU, value: u8) void {
    self.setA(self.subBytes(self.getA(), value, self.getCarry()));
}

fn addHL(self: *CPU, value: u16) void {
    const unaffected_flags = self.getF() & (Flags.S | Flags.Z | Flags.PV);
    self.setHL(self.addWords(self.getHL(), value, false));
    self.andFlags(~(Flags.S | Flags.Z | Flags.PV));
    self.orFlags(unaffected_flags);
}

fn adcHL(self: *CPU, value: u16) void {
    self.setHL(self.addWords(self.getHL(), value, self.getCarry()));
}

fn sbcHL(self: *CPU, value: u16) void {
    self.setHL(self.subWords(self.getHL(), value, self.getCarry()));
}

fn addIZ(self: *CPU, iz: *Pair, value: u16) void {
    const unaffected_flags = self.getF() & (Flags.S | Flags.Z | Flags.PV);
    iz.w = self.addWords(iz.w, value, false);
    self.andFlags(~(Flags.S | Flags.Z | Flags.PV));
    self.orFlags(unaffected_flags);
}

fn parityCheck(value: u8) bool {
    var result: u1 = 1;
    var i: u8 = 0;
    var v = value;
    while (i < 8) : (i += 1) {
        result ^= @truncate(u1, v);
        v >>= 1;
    }
    return @bitCast(bool, result);
}

fn bitwiseFlags(self: *CPU, value: u8) void {
    // sign and undoc flags
    self.orFlags(value & (Flags.S | Flags.X | Flags.Y));
    // zero flag
    if (value == 0) self.orFlags(Flags.Z);
    // parity flag
    if (parityCheck(value)) self.orFlags(Flags.PV);
}

fn andA(self: *CPU, value: u8) void {
    self.setA(self.getA() & value);
    self.setF(Flags.H);
    self.bitwiseFlags(self.getA());
}

fn orA(self: *CPU, value: u8) void {
    self.setA(self.getA() | value);
    self.setF(0);
    self.bitwiseFlags(self.getA());
}

fn xor(self: *CPU, value: u8) void {
    self.setA(self.getA() ^ value);
    self.setF(0);
    self.bitwiseFlags(self.getA());
}

fn cp(self: *CPU, value: u8) void {
    _ = self.subBytes(self.getA(), value, false);
    // xy flags take from operand instead of result
    self.andFlags(~(Flags.X | Flags.Y));
    self.orFlags(value & (Flags.X | Flags.Y));
}

fn inc(self: *CPU, value: u8) u8 {
    const carry = self.getF() & Flags.C;
    const result = self.addBytes(value, 1, false);
    // carry is not affected
    self.andFlags(~Flags.C);
    self.orFlags(carry);
    return result;
}

fn incMem(self: *CPU, addr: u16) void {
    self.writeByte(addr, self.inc(self.readByte(addr)));
}

fn dec(self: *CPU, value: u8) u8 {
    const carry = self.getF() & Flags.C;
    const result = self.subBytes(value, 1, false);
    // carry is not affected
    self.andFlags(~Flags.C);
    self.orFlags(carry);
    return result;
}

fn decMem(self: *CPU, addr: u16) void {
    self.writeByte(addr, self.dec(self.readByte(addr)));
}

fn daa(self: *CPU) void {
    var diff: u8 = 0;
    if (self.checkFlag(Flags.H) or self.getA() & 0x0f > 0x09) {
        diff += 0x06;
    }
    if (self.checkFlag(Flags.C) or self.getA() > 0x99) {
        self.orFlags(Flags.C);
        diff += 0x60;
    }

    if (self.checkFlag(Flags.N)) {
        if (self.getA() & 0x0f > 0x05) self.andFlags(~Flags.H);
        self.setA(self.getA() -% diff);
    } else {
        if (self.getA() & 0x0f > 0x09) {
            self.orFlags(Flags.H);
        } else {
            self.andFlags(~Flags.H);
        }
        self.setA(self.getA() +% diff);
    }

    self.andFlags(~(Flags.PV | Flags.S | Flags.X | Flags.Y | Flags.Z));
    self.bitwiseFlags(self.getA());
}

fn cpl(self: *CPU) void {
    self.setA(~self.getA());
    self.andFlags(~(Flags.X | Flags.Y));
    self.orFlags(Flags.H | Flags.N | (self.getA() & (Flags.X | Flags.Y)));
}

fn neg(self: *CPU) void {
    self.setA(self.subBytes(0, self.getA(), false));
}

fn ccf(self: *CPU) void {
    self.andFlags(~(Flags.H | Flags.N | Flags.X | Flags.Y));
    if (self.checkFlag(Flags.C)) self.orFlags(Flags.H);
    self.flipFlags(Flags.C);
    self.orFlags(self.getA() & (Flags.X | Flags.Y));
}

fn scf(self: *CPU) void {
    self.andFlags(~(Flags.H | Flags.N | Flags.X | Flags.Y));
    self.orFlags(Flags.C | (self.getA() & (Flags.X | Flags.Y)));
}

fn rlca(self: *CPU) void {
    self.andFlags(Flags.S | Flags.Z | Flags.PV | Flags.C);
    self.rlc(&self.af.b.h);
    self.orFlags(self.getA() & (Flags.X | Flags.Y));
}

fn rla(self: *CPU) void {
    self.andFlags(Flags.S | Flags.Z | Flags.PV | Flags.C);
    self.rl(&self.af.b.h);
    self.orFlags(self.getA() & (Flags.X | Flags.Y));
}

fn rrca(self: *CPU) void {
    self.andFlags(Flags.S | Flags.Z | Flags.PV | Flags.C);
    self.rrc(&self.af.b.h);
    self.orFlags(self.getA() & (Flags.X | Flags.Y));
}

fn rra(self: *CPU) void {
    self.andFlags(Flags.S | Flags.Z | Flags.PV | Flags.C);
    self.rr(&self.af.b.h);
    self.orFlags(self.getA() & (Flags.X | Flags.Y));
}

fn rlc(self: *CPU, reg: *u8) void {
    reg.* = (reg.* << 1) | (reg.* >> 7);
    self.andFlags(~Flags.C);
    self.orFlags(reg.* & Flags.C);
}

fn rrc(self: *CPU, reg: *u8) void {
    self.andFlags(~Flags.C);
    self.orFlags(reg.* & Flags.C);
    reg.* = (reg.* >> 1) | (reg.* << 7);
}

fn rl(self: *CPU, reg: *u8) void {
    const new_carry = reg.* >> 7;
    reg.* = (reg.* << 1) | (self.getF() & Flags.C);
    self.andFlags(~Flags.C);
    self.orFlags(new_carry);
}

fn rr(self: *CPU, reg: *u8) void {
    const new_carry = reg.* & Flags.C;
    reg.* = (reg.* >> 1) | (self.getF() << 7);
    self.andFlags(~Flags.C);
    self.orFlags(new_carry);
}

fn sla(self: *CPU, reg: *u8) void {
    self.setF(reg.* >> 7);
    reg.* <<= 1;
}

fn sra(self: *CPU, reg: *u8) void {
    self.setF(reg.* & Flags.C);
    reg.* = (reg.* >> 1) | (reg.* & 0x80);
}

fn sll(self: *CPU, reg: *u8) void {
    self.setF(reg.* >> 7);
    reg.* = (reg.* << 1) | 1;
}

fn srl(self: *CPU, reg: *u8) void {
    self.setF(reg.* & Flags.C);
    reg.* >>= 1;
}

fn jump(self: *CPU, addr: u16) void {
    self.pc = addr;
    self.wz = addr;
}

inline fn jumpIf(self: *CPU, condition: bool) void {
    self.wz = self.fetchWord();
    if (condition) self.pc = self.wz;
}

fn relJump(self: *CPU, relative: u8) void {
    self.pc +%= signedByte(relative);
    self.wz = self.pc;
}

inline fn relJumpIf(self: *CPU, condition: bool) void {
    const dest = self.fetchByte();
    if (condition) {
        self.relJump(dest);
        self.addCycles(5);
    }
}

fn djnz(self: *CPU) void {
    self.setB(self.getB() -% 1);
    self.relJumpIf(self.getB() != 0);
}

fn call(self: *CPU, addr: u16) void {
    self.push(self.pc);
    self.pc = addr;
    self.wz = addr;
}

inline fn callIf(self: *CPU, condition: bool) void {
    self.wz = self.fetchWord();
    if (condition) {
        self.push(self.pc);
        self.pc = self.wz;
        self.addCycles(7);
    }
}

fn ret(self: *CPU) void {
    self.pc = self.pop();
    self.wz = self.pc;
}

inline fn retIf(self: *CPU, condition: bool) void {
    if (condition) {
        self.ret();
        self.addCycles(6);
    }
}

fn inFromC(self: *CPU) u8 {
    const value = self.in(self.getBC());
    self.andFlags(Flags.C);
    self.bitwiseFlags(value);
    return value;
}

fn outToC(self: *CPU, value: u8) void {
    self.out(self.getBC(), value);
}

fn di(self: *CPU) void {
    self.iff1 = false;
    self.iff2 = false;
}

fn ei(self: *CPU) void {
    self.iff1 = true;
    self.iff2 = true;
    self.int_delay = true;
}

inline fn swap(p1: *Pair, p2: *Pair) void {
    const temp = p1.*;
    p1.* = p2.*;
    p2.* = temp;
}

fn exx(self: *CPU) void {
    swap(&self.bc, &self.bc2);
    swap(&self.de, &self.de2);
    swap(&self.hl, &self.hl2);
}

fn exSP(self: *CPU, p: *Pair) void {
    self.wz = self.readWord(self.sp);
    self.writeWord(self.sp, p.w);
    p.w = self.wz;
}

fn transferFlags(self: *CPU, value: u8) void {
    self.orFlags((value & Flags.X) | ((value << 4) & Flags.Y));
    if (self.getBC() != 0) self.orFlags(Flags.PV);
}

fn loadBlockByte(self: *CPU) void {
    const value = self.readHL();
    self.writeByte(self.getDE(), value);

    self.decBC();

    const sum = value +% self.getA();
    self.andFlags(Flags.S | Flags.Z | Flags.C);
    self.transferFlags(sum);
}

fn repeatBlockTransfer(self: *CPU) void {
    if (self.getBC() != 0) {
        self.pc -%= 2;
        self.addCycles(5);
        self.wz = self.pc +% 1;
    }
}

fn ldi(self: *CPU) void {
    self.loadBlockByte();
    self.incDE();
    self.incHL();
}

fn ldir(self: *CPU) void {
    self.ldi();
    self.repeatBlockTransfer();
}

fn ldd(self: *CPU) void {
    self.loadBlockByte();
    self.decDE();
    self.decHL();
}

fn lddr(self: *CPU) void {
    self.ldd();
    self.repeatBlockTransfer();
}

fn compareBlockByte(self: *CPU) void {
    const carry = self.getF() & Flags.C;

    const result = self.subBytes(self.getA(), self.readHL(), false);
    self.decBC();

    self.andFlags(~(Flags.X | Flags.Y | Flags.PV | Flags.C));
    self.transferFlags(result -% @boolToInt(self.checkFlag(Flags.H)));
    self.orFlags(carry);
}

fn repeatBlockCompare(self: *CPU) void {
    if (self.getBC() != 0 and !self.checkFlag(Flags.Z)) {
        self.pc -%= 2;
        self.addCycles(5);
        self.wz = self.pc +% 1;
    } else {
        self.wz +%= 1;
    }
}

fn cpi(self: *CPU) void {
    self.compareBlockByte();
    self.incHL();
    self.wz +%= 1;
}

fn cpir(self: *CPU) void {
    self.cpi();
    self.repeatBlockCompare();
}

fn cpd(self: *CPU) void {
    self.compareBlockByte();
    self.decHL();
    self.wz -%= 1;
}

fn cpdr(self: *CPU) void {
    self.cpd();
    self.repeatBlockCompare();
}

fn ioBlockByte(self: *CPU, value: u8, reg: u8) void {
    self.setB(self.getB() -% 1);
    self.setF(self.getB() & (Flags.S | Flags.X | Flags.Y));
    if (self.getB() == 0) self.orFlags(Flags.Z);
    if (value & 0x80 != 0) self.orFlags(Flags.N);
    const k = @as(u16, value) + reg;
    if (k > 255) self.orFlags(Flags.C | Flags.H);
    if (parityCheck(@truncate(u3, k) ^ self.getB())) {
        self.orFlags(Flags.PV);
    }
}

fn inBlockByte(self: *CPU) u8 {
    const value = self.in(word(self.getB() -% 1, self.getC()));
    self.writeHL(value);
    return value;
}

fn ini(self: *CPU) void {
    const value = self.inBlockByte();
    self.incHL();
    self.wz = self.getBC() +% 1;
    self.ioBlockByte(value, self.getC() +% 1);
}

fn inir(self: *CPU) void {
    self.ini();
    self.repeatBlockTransfer();
}

fn ind(self: *CPU) void {
    const value = self.inBlockByte();
    self.decHL();
    self.wz = self.getBC() -% 1;
    self.ioBlockByte(value, self.getC() -% 1);
}

fn indr(self: *CPU) void {
    self.ind();
    self.repeatBlockTransfer();
}

fn outBlockByte(self: *CPU) u8 {
    const value = self.readHL();
    self.outToC(value);
    return value;
}

fn outi(self: *CPU) void {
    const value = self.outBlockByte();
    self.incHL();
    self.ioBlockByte(value, self.getL());
    self.wz = self.getBC() +% 1;
}

fn otir(self: *CPU) void {
    self.outi();
    self.repeatBlockTransfer();
}

fn outd(self: *CPU) void {
    const value = self.outBlockByte();
    self.decHL();
    self.ioBlockByte(value, self.getL());
    self.wz = self.getBC() -% 1;
}

fn otdr(self: *CPU) void {
    self.outd();
    self.repeatBlockTransfer();
}

fn rld(self: *CPU) void {
    const value = self.readHL();
    self.writeHL((value << 4) | (self.getA() & 0x0f));
    self.setA((self.getA() & 0xf0) | (value >> 4));
    self.andFlags(Flags.C);
    self.bitwiseFlags(self.getA());
    self.wz = self.getHL() +% 1;
}

fn rrd(self: *CPU) void {
    const value = self.readHL();
    self.writeHL((self.getA() << 4) | (value >> 4));
    self.setA((self.getA() & 0xf0) | (value & 0x0f));
    self.andFlags(Flags.C);
    self.bitwiseFlags(self.getA());
    self.wz = self.getHL() +% 1;
}

fn reti(self: *CPU) void {
    self.retn();
    self.interface.reti();
}

fn retn(self: *CPU) void {
    self.iff1 = self.iff2;
    self.ret();
}

fn inA(self: *CPU, port: u8) void {
    self.wz = (self.getAF() & 0xff00) +% port +% 1;
    self.setA(self.in(word(self.getA(), port)));
}

fn inAFromC(self: *CPU) void {
    self.setA(self.inFromC());
    self.wz = self.getBC() +% 1;
}

fn outA(self: *CPU, port: u8) void {
    self.wz = word(self.getA(), port +% 1);
    self.out(word(self.getA(), port), self.getA());
}

fn outAToC(self: *CPU) void {
    self.outToC(self.getA());
    self.wz = self.getBC() +% 1;
}

fn setAToIR(self: *CPU, value: u8) void {
    self.setA(value);
    self.andFlags(Flags.C);
    if (self.getA() == 0) self.orFlags(Flags.Z);
    if (self.iff2) self.orFlags(Flags.PV);
    self.orFlags(self.getA() & (Flags.S | Flags.X | Flags.Y));
    self.ld_ir = true;
}

inline fn irBug(self: *CPU) void {
    if (self.ld_ir) self.andFlags(~Flags.PV);
}

inline fn unhalt(self: *CPU) void {
    self.halted = false;
}

/// Step the processor.
pub fn step(self: *CPU) void {
    self.ld_ir = false;
    switch (self.index_type) {
        .HL => if (self.halted) {
            self.main(0);
        } else {
            self.main(self.fetchByte());
        },
        .IX => {
            self.index_type = .HL;
            self.indexed(&self.ix);
        },
        .IY => {
            self.index_type = .HL;
            self.indexed(&self.iy);
        },
    }
    if (self.index_type == .HL) {
        // instruction is over, stop reading instruction bytes from data bus if we are for im 0
        self.int_read = false;
    } else {
        // cannot execute an interrupt in the middle of an iz-prefixed instruction
        self.int_delay = true;
    }
}

// main opcodes (non-prefixed)
fn main(self: *CPU, opcode: u8) void {
    self.refresh();
    self.addCycles(CYCLE_COUNTS[opcode]);
    // we can take interrupts after this execution
    self.int_delay = false;
    // perform opcode operation
    switch (opcode) {
        // 8-bit load group
        0x7f => self.setA(self.getA()),                   // ld   a,a
        0x47 => self.setB(self.getA()),                   // ld   b,a
        0x4f => self.setC(self.getA()),                   // ld   c,a
        0x57 => self.setD(self.getA()),                   // ld   d,a
        0x5f => self.setE(self.getA()),                   // ld   e,a
        0x67 => self.setH(self.getA()),                   // ld   h,a
        0x6f => self.setL(self.getA()),                   // ld   l,a
        0x78 => self.setA(self.getB()),                   // ld   a,b
        0x40 => self.setB(self.getB()),                   // ld   b,b
        0x48 => self.setC(self.getB()),                   // ld   c,b
        0x50 => self.setD(self.getB()),                   // ld   d,b
        0x58 => self.setE(self.getB()),                   // ld   e,b
        0x60 => self.setH(self.getB()),                   // ld   h,b
        0x68 => self.setL(self.getB()),                   // ld   l,b
        0x79 => self.setA(self.getC()),                   // ld   a,c
        0x41 => self.setB(self.getC()),                   // ld   b,c
        0x49 => self.setC(self.getC()),                   // ld   c,c
        0x51 => self.setD(self.getC()),                   // ld   d,c
        0x59 => self.setE(self.getC()),                   // ld   e,c
        0x61 => self.setH(self.getC()),                   // ld   h,c
        0x69 => self.setL(self.getC()),                   // ld   l,c
        0x7a => self.setA(self.getD()),                   // ld   a,d
        0x42 => self.setB(self.getD()),                   // ld   b,d
        0x4a => self.setC(self.getD()),                   // ld   c,d
        0x52 => self.setD(self.getD()),                   // ld   d,d
        0x5a => self.setE(self.getD()),                   // ld   e,d
        0x62 => self.setH(self.getD()),                   // ld   h,d
        0x6a => self.setL(self.getD()),                   // ld   l,d
        0x7b => self.setA(self.getE()),                   // ld   a,e
        0x43 => self.setB(self.getE()),                   // ld   b,e
        0x4b => self.setC(self.getE()),                   // ld   c,e
        0x53 => self.setD(self.getE()),                   // ld   d,e
        0x5b => self.setE(self.getE()),                   // ld   e,e
        0x63 => self.setH(self.getE()),                   // ld   h,e
        0x6b => self.setL(self.getE()),                   // ld   l,e
        0x7c => self.setA(self.getH()),                   // ld   a,h
        0x44 => self.setB(self.getH()),                   // ld   b,h
        0x4c => self.setC(self.getH()),                   // ld   c,h
        0x54 => self.setD(self.getH()),                   // ld   d,h
        0x5c => self.setE(self.getH()),                   // ld   e,h
        0x64 => self.setH(self.getH()),                   // ld   h,h
        0x6c => self.setL(self.getH()),                   // ld   l,h
        0x7d => self.setA(self.getL()),                   // ld   a,l
        0x45 => self.setB(self.getL()),                   // ld   b,l
        0x4d => self.setC(self.getL()),                   // ld   c,l
        0x55 => self.setD(self.getL()),                   // ld   d,l
        0x5d => self.setE(self.getL()),                   // ld   e,l
        0x65 => self.setH(self.getL()),                   // ld   h,l
        0x6d => self.setL(self.getL()),                   // ld   l,l
        0x3e => self.setA(self.fetchByte()),              // ld   a,*
        0x06 => self.setB(self.fetchByte()),              // ld   b,*
        0x0e => self.setC(self.fetchByte()),              // ld   c,*
        0x16 => self.setD(self.fetchByte()),              // ld   d,*
        0x1e => self.setE(self.fetchByte()),              // ld   e,*
        0x26 => self.setH(self.fetchByte()),              // ld   h,*
        0x2e => self.setL(self.fetchByte()),              // ld   l,*
        0x7e => self.setA(self.readHL()),                 // ld   a,(hl)
        0x46 => self.setB(self.readHL()),                 // ld   b,(hl)
        0x4e => self.setC(self.readHL()),                 // ld   c,(hl)
        0x56 => self.setD(self.readHL()),                 // ld   d,(hl)
        0x5e => self.setE(self.readHL()),                 // ld   e,(hl)
        0x66 => self.setH(self.readHL()),                 // ld   h,(hl)
        0x6e => self.setL(self.readHL()),                 // ld   l,(hl)
        0x77 => self.writeHL(self.getA()),                // ld   (hl),a
        0x70 => self.writeHL(self.getB()),                // ld   (hl),b
        0x71 => self.writeHL(self.getC()),                // ld   (hl),c
        0x72 => self.writeHL(self.getD()),                // ld   (hl),d
        0x73 => self.writeHL(self.getE()),                // ld   (hl),e
        0x74 => self.writeHL(self.getH()),                // ld   (hl),h
        0x75 => self.writeHL(self.getL()),                // ld   (hl),l
        0x36 => self.writeHL(self.fetchByte()),           // ld   (hl),*
        0x0a => self.readAWithMemPtr(self.getBC()),       // ld   a,(bc)
        0x1a => self.readAWithMemPtr(self.getDE()),       // ld   a,(de)
        0x3a => self.readAWithMemPtr(self.fetchWord()),   // ld   a,(**)
        0x02 => self.writeAWithMemPtr(self.getBC()),      // ld   (bc),a
        0x12 => self.writeAWithMemPtr(self.getDE()),      // ld   (de),a
        0x32 => self.writeAWithMemPtr(self.fetchWord()),  // ld   (**),a
        // 16-bit load group
        0x01 => self.setBC(self.fetchWord()),             // ld   bc,**
        0x11 => self.setDE(self.fetchWord()),             // ld   de,**
        0x21 => self.setHL(self.fetchWord()),             // ld   hl,**
        0x31 => self.sp = self.fetchWord(),               // ld   sp,**
        0x2a => self.setHL(self.readPairWithMemPtr()),    // ld   hl,(**)
        0x22 => self.writePairWithMemPtr(self.getHL()),   // ld   (**),hl
        0xf9 => self.sp = self.getHL(),                   // ld   sp,hl
        0xf5 => self.push(self.getAF()),                  // push af
        0xc5 => self.push(self.getBC()),                  // push bc
        0xd5 => self.push(self.getDE()),                  // push de
        0xe5 => self.push(self.getHL()),                  // push hl
        0xf1 => self.setAF(self.pop()),                   // pop  af
        0xc1 => self.setBC(self.pop()),                   // pop  bc
        0xd1 => self.setDE(self.pop()),                   // pop  de
        0xe1 => self.setHL(self.pop()),                   // pop  hl
        // exchange, block transfer, and search group
        0xeb => swap(&self.de, &self.hl),                 // ex   de,hl
        0x08 => swap(&self.af, &self.af2),                // ex   af,af'
        0xd9 => self.exx(),                               // exx
        0xe3 => self.exSP(&self.hl),                      // ex   (sp),hl
        // 8-bit arithmetic group
        0x87 => self.addA(self.getA()),                   // add  a,a
        0x80 => self.addA(self.getB()),                   // add  a,b
        0x81 => self.addA(self.getC()),                   // add  a,c
        0x82 => self.addA(self.getD()),                   // add  a,d
        0x83 => self.addA(self.getE()),                   // add  a,e
        0x84 => self.addA(self.getH()),                   // add  a,h
        0x85 => self.addA(self.getL()),                   // add  a,l
        0xc6 => self.addA(self.fetchByte()),              // add  a,*
        0x86 => self.addA(self.readHL()),                 // add  a,(hl)
        0x8f => self.adcA(self.getA()),                   // adc  a,a
        0x88 => self.adcA(self.getB()),                   // adc  a,b
        0x89 => self.adcA(self.getC()),                   // adc  a,c
        0x8a => self.adcA(self.getD()),                   // adc  a,d
        0x8b => self.adcA(self.getE()),                   // adc  a,e
        0x8c => self.adcA(self.getH()),                   // adc  a,h
        0x8d => self.adcA(self.getL()),                   // adc  a,l
        0xce => self.adcA(self.fetchByte()),              // adc  a,*
        0x8e => self.adcA(self.readHL()),                 // adc  a,(hl)
        0x97 => self.subA(self.getA()),                   // sub  a
        0x90 => self.subA(self.getB()),                   // sub  b
        0x91 => self.subA(self.getC()),                   // sub  c
        0x92 => self.subA(self.getD()),                   // sub  d
        0x93 => self.subA(self.getE()),                   // sub  e
        0x94 => self.subA(self.getH()),                   // sub  h
        0x95 => self.subA(self.getL()),                   // sub  l
        0xd6 => self.subA(self.fetchByte()),              // sub  *
        0x96 => self.subA(self.readHL()),                 // sub  (hl)
        0x9f => self.sbcA(self.getA()),                   // sbc  a,a
        0x98 => self.sbcA(self.getB()),                   // sbc  a,b
        0x99 => self.sbcA(self.getC()),                   // sbc  a,c
        0x9a => self.sbcA(self.getD()),                   // sbc  a,d
        0x9b => self.sbcA(self.getE()),                   // sbc  a,e
        0x9c => self.sbcA(self.getH()),                   // sbc  a,h
        0x9d => self.sbcA(self.getL()),                   // sbc  a,l
        0xde => self.sbcA(self.fetchByte()),              // sbc  a,*
        0x9e => self.sbcA(self.readHL()),                 // sbc  a,(hl)
        0xa7 => self.andA(self.getA()),                   // and  a
        0xa0 => self.andA(self.getB()),                   // and  b
        0xa1 => self.andA(self.getC()),                   // and  c
        0xa2 => self.andA(self.getD()),                   // and  d
        0xa3 => self.andA(self.getE()),                   // and  e
        0xa4 => self.andA(self.getH()),                   // and  h
        0xa5 => self.andA(self.getL()),                   // and  l
        0xe6 => self.andA(self.fetchByte()),              // and  *
        0xa6 => self.andA(self.readHL()),                 // and  (hl)
        0xb7 => self.orA(self.getA()),                    // or   a
        0xb0 => self.orA(self.getB()),                    // or   b
        0xb1 => self.orA(self.getC()),                    // or   c
        0xb2 => self.orA(self.getD()),                    // or   d
        0xb3 => self.orA(self.getE()),                    // or   e
        0xb4 => self.orA(self.getH()),                    // or   h
        0xb5 => self.orA(self.getL()),                    // or   l
        0xf6 => self.orA(self.fetchByte()),               // or   *
        0xb6 => self.orA(self.readHL()),                  // or   (hl)
        0xaf => self.xor(self.getA()),                    // xor  a
        0xa8 => self.xor(self.getB()),                    // xor  b
        0xa9 => self.xor(self.getC()),                    // xor  c
        0xaa => self.xor(self.getD()),                    // xor  d
        0xab => self.xor(self.getE()),                    // xor  e
        0xac => self.xor(self.getH()),                    // xor  h
        0xad => self.xor(self.getL()),                    // xor  l
        0xee => self.xor(self.fetchByte()),               // xor  *
        0xae => self.xor(self.readHL()),                  // xor  (hl)
        0xbf => self.cp(self.getA()),                     // cp   a
        0xb8 => self.cp(self.getB()),                     // cp   b
        0xb9 => self.cp(self.getC()),                     // cp   c
        0xba => self.cp(self.getD()),                     // cp   d
        0xbb => self.cp(self.getE()),                     // cp   e
        0xbc => self.cp(self.getH()),                     // cp   h
        0xbd => self.cp(self.getL()),                     // cp   l
        0xfe => self.cp(self.fetchByte()),                // cp   *
        0xbe => self.cp(self.readHL()),                   // cp   (hl)
        0x3c => self.setA(self.inc(self.getA())),         // inc  a
        0x04 => self.setB(self.inc(self.getB())),         // inc  b
        0x0c => self.setC(self.inc(self.getC())),         // inc  c
        0x14 => self.setD(self.inc(self.getD())),         // inc  d
        0x1c => self.setE(self.inc(self.getE())),         // inc  e
        0x24 => self.setH(self.inc(self.getH())),         // inc  h
        0x2c => self.setL(self.inc(self.getL())),         // inc  l
        0x34 => self.incMem(self.getHL()),                // inc  (hl)
        0x3d => self.setA(self.dec(self.getA())),         // dec  a
        0x05 => self.setB(self.dec(self.getB())),         // dec  b
        0x0d => self.setC(self.dec(self.getC())),         // dec  c
        0x15 => self.setD(self.dec(self.getD())),         // dec  d
        0x1d => self.setE(self.dec(self.getE())),         // dec  e
        0x25 => self.setH(self.dec(self.getH())),         // dec  h
        0x2d => self.setL(self.dec(self.getL())),         // dec  l
        0x35 => self.decMem(self.getHL()),                // dec  (hl)
        // general-purpose arithmetic and cpu control groups
        0x27 => self.daa(),                               // daa
        0x2f => self.cpl(),                               // cpl
        0x3f => self.ccf(),                               // ccf
        0x37 => self.scf(),                               // scf
        0x00 => {},                                       // nop
        0x76 => self.halted = true,                       // halt
        0xf3 => self.di(),                                // di
        0xfb => self.ei(),                                // ei
        // 16-bit arithmetic group
        0x09 => self.addHL(self.getBC()),                 // add  hl,bc
        0x19 => self.addHL(self.getDE()),                 // add  hl,de
        0x29 => self.addHL(self.getHL()),                 // add  hl,hl
        0x39 => self.addHL(self.sp),                      // add  hl,sp
        0x03 => self.incBC(),                             // inc  bc
        0x13 => self.incDE(),                             // inc  de
        0x23 => self.incHL(),                             // inc  hl
        0x33 => self.sp +%= 1,                            // inc  sp
        0x0b => self.decBC(),                             // dec  bc
        0x1b => self.decDE(),                             // dec  de
        0x2b => self.decHL(),                             // dec  hl
        0x3b => self.sp -%= 1,                            // dec  sp
        // rotate and shift group
        0x07 => self.rlca(),                              // rlca
        0x17 => self.rla(),                               // rla
        0x0f => self.rrca(),                              // rrca
        0x1f => self.rra(),                               // rra
        // jump group
        0xc3 => self.jump(self.fetchWord()),              // jp   **
        0xc2 => self.jumpIf(!self.checkFlag(Flags.Z)),    // jp   nz,**
        0xca => self.jumpIf(self.checkFlag(Flags.Z)),     // jp   z,**
        0xd2 => self.jumpIf(!self.checkFlag(Flags.C)),    // jp   nc,**
        0xda => self.jumpIf(self.checkFlag(Flags.C)),     // jp   c,**
        0xe2 => self.jumpIf(!self.checkFlag(Flags.PV)),   // jp   po,**
        0xea => self.jumpIf(self.checkFlag(Flags.PV)),    // jp   pe,**
        0xf2 => self.jumpIf(!self.checkFlag(Flags.S)),    // jp   p,**
        0xfa => self.jumpIf(self.checkFlag(Flags.S)),     // jp   m,**
        0x18 => self.relJump(self.fetchByte()),           // jr
        0x20 => self.relJumpIf(!self.checkFlag(Flags.Z)), // jr   nz,*
        0x28 => self.relJumpIf(self.checkFlag(Flags.Z)),  // jr   z,*
        0x30 => self.relJumpIf(!self.checkFlag(Flags.C)), // jr   nc,*
        0x38 => self.relJumpIf(self.checkFlag(Flags.C)),  // jr   c,*
        0xe9 => self.pc = self.getHL(),                   // jp   (hl)
        0x10 => self.djnz(),                              // djnz e
        // call and return group
        0xcd => self.call(self.fetchWord()),              // call **
        0xc4 => self.callIf(!self.checkFlag(Flags.Z)),    // call nz,**
        0xcc => self.callIf(self.checkFlag(Flags.Z)),     // call z,**
        0xd4 => self.callIf(!self.checkFlag(Flags.C)),    // call nc,**
        0xdc => self.callIf(self.checkFlag(Flags.C)),     // call c,**
        0xe4 => self.callIf(!self.checkFlag(Flags.PV)),   // call po,**
        0xec => self.callIf(self.checkFlag(Flags.PV)),    // call pe,**
        0xf4 => self.callIf(!self.checkFlag(Flags.S)),    // call p,**
        0xfc => self.callIf(self.checkFlag(Flags.S)),     // call m,**
        0xc9 => self.ret(),                               // ret
        0xc0 => self.retIf(!self.checkFlag(Flags.Z)),     // ret  nz
        0xc8 => self.retIf(self.checkFlag(Flags.Z)),      // ret  z
        0xd0 => self.retIf(!self.checkFlag(Flags.C)),     // ret  nc
        0xd8 => self.retIf(self.checkFlag(Flags.C)),      // ret  c
        0xe0 => self.retIf(!self.checkFlag(Flags.PV)),    // ret  po
        0xe8 => self.retIf(self.checkFlag(Flags.PV)),     // ret  pe
        0xf0 => self.retIf(!self.checkFlag(Flags.S)),     // ret  p
        0xf8 => self.retIf(self.checkFlag(Flags.S)),      // ret  m
        0xc7 => self.call(0x00),                          // rst  00h
        0xcf => self.call(0x08),                          // rst  08h
        0xd7 => self.call(0x10),                          // rst  10h
        0xdf => self.call(0x18),                          // rst  18h
        0xe7 => self.call(0x20),                          // rst  20h
        0xef => self.call(0x28),                          // rst  28h
        0xf7 => self.call(0x30),                          // rst  30h
        0xff => self.call(0x38),                          // rst  38h
        // input and output group
        0xdb => self.inA(self.fetchByte()),               // in   a,(*)
        0xd3 => self.outA(self.fetchByte()),              // out  (*),a
        // bit instructions
        0xcb => self.bits(self.getHL(), false),
        // ix instructions
        0xdd => self.index_type = .IX,
        // extended instructions
        0xed => self.extended(),
        // iy instructions
        0xfd => self.index_type = .IY,
    }
}

/// bit opcodes (cb prefixed)
fn bits(self: *CPU, address: u16, comptime is_indexed: bool) void {
    // fetch and refresh
    const opcode = self.fetchByte();
    self.refresh();
    // 4 cycles for instruction fetch
    self.addCycles(4);
    // opcode parameters
    const operand = @truncate(u3, opcode);
    const bit_index = @truncate(u3, opcode >> 3);
    const group = @intCast(u2, opcode >> 6);
    const bitmask = @as(u8, 1) << bit_index;
    // get register pointer (memory values use local variable)
    // based on implementation in https://github.com/superzazu/z80
    var mem: u8 = undefined;
    const reg = switch (operand) {
        0 => &self.bc.b.h, 1 => &self.bc.b.l,
        2 => &self.de.b.h, 3 => &self.de.b.l,
        4 => &self.hl.b.h, 5 => &self.hl.b.l,
        6 => &mem,         7 => &self.af.b.h,
    };
    if (is_indexed or reg == &mem) {
        // in indexed mode, data is always read from (iz+*), even if a register is specified.
        // this will overwrite the register value.
        // in non-indexed mode, data is only read when the operand is (hl).
        reg.* = self.readByte(address);
    }
    // select operation from group
    switch (group) {
        // shift/rotate
        0 => {
            self.andFlags(Flags.C);
            switch (bit_index) {
                0 => self.rlc(reg),
                1 => self.rrc(reg),
                2 => self.rl(reg),
                3 => self.rr(reg),
                4 => self.sla(reg),
                5 => self.sra(reg),
                6 => self.sll(reg),
                7 => self.srl(reg),
            }
            self.bitwiseFlags(reg.*);
        },
        // bit
        1 => {
            const result = reg.* & bitmask;
            self.andFlags(Flags.C);
            self.orFlags(Flags.H | (result & Flags.S) | (reg.* & (Flags.X | Flags.Y)));
            if (result == 0) self.orFlags(Flags.Z | Flags.PV);
            if (is_indexed) reg.* = result;
        },
        2 => reg.* &= ~bitmask, // res
        3 => reg.* |= bitmask,  // set
    }

    if (is_indexed) {
        self.addCycles(8);
    } else if (reg == &mem) {
        self.addCycles(4);
    } else {
        return;
    }

    if (group == 1) {
        // if bit operation, override xy flags with high byte of memptr
        self.andFlags(~(Flags.X | Flags.Y));
        self.orFlags(hiByte(self.wz) & (Flags.X | Flags.Y));
    } else {
        // otherwise, write memory and add three cycles
        self.writeByte(address, reg.*);
        self.addCycles(3);
    }
}

/// indexed opcodes (ix or iy)
fn indexed(self: *CPU, iz: *Pair) void {
    const opcode = self.fetchByte();
    const old_r = self.r;
    self.refresh();
    self.addCycles(IZ_CYCLE_COUNTS[opcode]);

    switch (opcode) {
        // 8-bit load group
        0x67 => iz.b.h = self.getA(),                             // ld   izh,a
        0x6f => iz.b.l = self.getA(),                             // ld   izl,a
        0x60 => iz.b.h = self.getB(),                             // ld   izh,b
        0x68 => iz.b.l = self.getB(),                             // ld   izl,b
        0x61 => iz.b.h = self.getC(),                             // ld   izh,c
        0x69 => iz.b.l = self.getC(),                             // ld   izl,c
        0x62 => iz.b.h = self.getD(),                             // ld   izh,d
        0x6a => iz.b.l = self.getD(),                             // ld   izl,d
        0x63 => iz.b.h = self.getE(),                             // ld   izh,e
        0x6b => iz.b.l = self.getE(),                             // ld   izl,e
        0x7c => self.setA(iz.b.h),                                // ld   a,izh
        0x44 => self.setB(iz.b.h),                                // ld   b,izh
        0x4c => self.setC(iz.b.h),                                // ld   c,izh
        0x54 => self.setD(iz.b.h),                                // ld   d,izh
        0x5c => self.setE(iz.b.h),                                // ld   e,izh
        0x64 => iz.b.h = iz.b.h,                                  // ld   izh,izh
        0x6c => iz.b.l = iz.b.h,                                  // ld   izl,izh
        0x7d => self.setA(iz.b.l),                                // ld   a,izl
        0x45 => self.setB(iz.b.l),                                // ld   b,izl
        0x4d => self.setC(iz.b.l),                                // ld   c,izl
        0x55 => self.setD(iz.b.l),                                // ld   d,izl
        0x5d => self.setE(iz.b.l),                                // ld   e,izl
        0x65 => iz.b.h = iz.b.l,                                  // ld   izh,izl
        0x6d => iz.b.l = iz.b.l,                                  // ld   izl,izl
        0x26 => iz.b.h = self.fetchByte(),                        // ld   izh,*
        0x2e => iz.b.l = self.fetchByte(),                        // ld   izl,*
        0x7e => self.setA(self.readIZD(iz.w)),                    // ld   a,(iz+*)
        0x46 => self.setB(self.readIZD(iz.w)),                    // ld   b,(iz+*)
        0x4e => self.setC(self.readIZD(iz.w)),                    // ld   c,(iz+*)
        0x56 => self.setD(self.readIZD(iz.w)),                    // ld   d,(iz+*)
        0x5e => self.setE(self.readIZD(iz.w)),                    // ld   e,(iz+*)
        0x66 => self.setH(self.readIZD(iz.w)),                    // ld   h,(iz+*)
        0x6e => self.setL(self.readIZD(iz.w)),                    // ld   l,(iz+*)
        0x77 => self.writeByte(self.izd(iz.w), self.getA()),      // ld   (iz+*),a
        0x70 => self.writeByte(self.izd(iz.w), self.getB()),      // ld   (iz+*),b
        0x71 => self.writeByte(self.izd(iz.w), self.getC()),      // ld   (iz+*),c
        0x72 => self.writeByte(self.izd(iz.w), self.getD()),      // ld   (iz+*),d
        0x73 => self.writeByte(self.izd(iz.w), self.getE()),      // ld   (iz+*),e
        0x74 => self.writeByte(self.izd(iz.w), self.getH()),      // ld   (iz+*),h
        0x75 => self.writeByte(self.izd(iz.w), self.getL()),      // ld   (iz+*),l
        0x36 => self.writeByte(self.izd(iz.w), self.fetchByte()), // ld   (iz+*),*
        // 16-bit load group
        0x21 => iz.w = self.fetchWord(),                          // ld   iz,**
        0x2a => iz.w = self.readPairWithMemPtr(),                 // ld   iz,(**)
        0x22 => self.writePairWithMemPtr(iz.w),                   // ld   (**),iz
        0xf9 => self.sp = iz.w,                                   // ld   sp,iz
        0xe5 => self.push(iz.w),                                  // push iz
        0xe1 => iz.w = self.pop(),                                // pop  iz
        // exchange, block transfer, and search group
        0xe3 => self.exSP(iz),                                    // ex   (sp),iz
        // 8-bit arithmetic group
        0x84 => self.addA(iz.b.h),                                // add  a,izh
        0x85 => self.addA(iz.b.l),                                // add  a,izl
        0x86 => self.addA(self.readIZD(iz.w)),                    // add  a,(iz+*)
        0x8c => self.adcA(iz.b.h),                                // adc  a,izh
        0x8d => self.adcA(iz.b.l),                                // adc  a,izl
        0x8e => self.adcA(self.readIZD(iz.w)),                    // adc  a,(iz+*)
        0x94 => self.subA(iz.b.h),                                // sub   izh
        0x95 => self.subA(iz.b.l),                                // sub   izl
        0x96 => self.subA(self.readIZD(iz.w)),                    // sub   (iz+*)
        0x9c => self.sbcA(iz.b.h),                                // sbc  a,izh
        0x9d => self.sbcA(iz.b.l),                                // sbc  a,izl
        0x9e => self.sbcA(self.readIZD(iz.w)),                    // sbc  a,(iz+*)
        0xa4 => self.andA(iz.b.h),                                // and  izh
        0xa5 => self.andA(iz.b.l),                                // and  izl
        0xa6 => self.andA(self.readIZD(iz.w)),                    // and  (iz+*)
        0xb4 => self.orA(iz.b.h),                                 // or   izh
        0xb5 => self.orA(iz.b.l),                                 // or   izl
        0xb6 => self.orA(self.readIZD(iz.w)),                     // or   (iz+*)
        0xac => self.xor(iz.b.h),                                 // xor  izh
        0xad => self.xor(iz.b.l),                                 // xor  izl
        0xae => self.xor(self.readIZD(iz.w)),                     // xor  (iz+*)
        0xbc => self.cp(iz.b.h),                                  // cp   izh
        0xbd => self.cp(iz.b.l),                                  // cp   izl
        0xbe => self.cp(self.readIZD(iz.w)),                      // cp   (iz+*)
        0x24 => iz.b.h = self.inc(iz.b.h),                        // inc  izh
        0x2c => iz.b.l = self.inc(iz.b.l),                        // inc  izl
        0x34 => self.incMem(self.izd(iz.w)),                      // inc  (iz+*)
        0x25 => iz.b.h = self.dec(iz.b.h),                        // dec  izh
        0x2d => iz.b.l = self.dec(iz.b.l),                        // dec  izl
        0x35 => self.decMem(self.izd(iz.w)),                      // dec  (iz+*)
        // 16-bit arithmetic group
        0x09 => self.addIZ(iz, self.getBC()),                     // add  iz,bc
        0x19 => self.addIZ(iz, self.getDE()),                     // add  iz,de
        0x29 => self.addIZ(iz, iz.w),                             // add  iz,iz
        0x39 => self.addIZ(iz, self.sp),                          // add  iz,sp
        0x23 => iz.w +%= 1,                                       // inc  iz
        0x2b => iz.w -%= 1,                                       // dec  iz
        // jump group
        0xe9 => self.pc = iz.w,                                   // jp   (iz)
        // bit instructions
        0xcb => self.bits(self.izd(iz.w), true),
        // all other opcodes are identical to base opcodes
        else => {
            // undo refresh operation
            self.r = old_r;
            // execute from main opcode table
            self.main(opcode);
        },
    }
}

/// extended opcodes (ed-prefixed)
fn extended(self: *CPU) void {
    const opcode = self.fetchByte();
    self.refresh();
    self.addCycles(ED_CYCLE_COUNTS[opcode]);

    switch (opcode) {
        // 8-bit load group
        0x57 => self.setAToIR(self.i),              // ld   a,i
        0x5f => self.setAToIR(self.r),              // ld   a,r
        0x47 => self.i = self.getA(),               // ld   i,a
        0x4f => self.r = self.getA(),               // ld   r,a
        // 16-bit load group
        0x4b => self.setBC(self.readWordMemPtr()),  // ld   bc,(**)
        0x5b => self.setDE(self.readWordMemPtr()),  // ld   de,(**)
        0x6b => self.setHL(self.readWordMemPtr()),  // ld   hl,(**)
        0x7b => self.sp = self.readWordMemPtr(),    // ld   sp,(**)
        0x43 => self.writeWordMemPtr(self.getBC()), // ld   (**),bc
        0x53 => self.writeWordMemPtr(self.getDE()), // ld   (**),de
        0x63 => self.writeWordMemPtr(self.getHL()), // ld   (**),hl
        0x73 => self.writeWordMemPtr(self.sp),      // ld   (**),sp
        // exchange, block transfer, and search group
        0xa0 => self.ldi(),                         // ldi
        0xb0 => self.ldir(),                        // ldir
        0xa8 => self.ldd(),                         // ldd
        0xb8 => self.lddr(),                        // lddr
        0xa1 => self.cpi(),                         // cpi
        0xb1 => self.cpir(),                        // cpir
        0xa9 => self.cpd(),                         // cpd
        0xb9 => self.cpdr(),                        // cpdr
        // general-purpose arithmetic and cpu control groups
        0x44 => self.neg(),                         // neg
        0x4c => self.neg(),                         // neg
        0x54 => self.neg(),                         // neg
        0x5c => self.neg(),                         // neg
        0x64 => self.neg(),                         // neg
        0x6c => self.neg(),                         // neg
        0x74 => self.neg(),                         // neg
        0x7c => self.neg(),                         // neg
        0x46 => self.im = .Mode0,                   // im   0
        0x4e => self.im = .Mode0,                   // im   0
        0x66 => self.im = .Mode0,                   // im   0
        0x6e => self.im = .Mode0,                   // im   0
        0x56 => self.im = .Mode1,                   // im   1
        0x76 => self.im = .Mode1,                   // im   1
        0x5e => self.im = .Mode2,                   // im   2
        0x7e => self.im = .Mode2,                   // im   2
        // 16-bit arithmetic group
        0x4a => self.adcHL(self.getBC()),           // adc hl,bc
        0x5a => self.adcHL(self.getDE()),           // adc hl,de
        0x6a => self.adcHL(self.getHL()),           // adc hl,hl
        0x7a => self.adcHL(self.sp),                // adc hl,sp
        0x42 => self.sbcHL(self.getBC()),           // sbc hl,bc
        0x52 => self.sbcHL(self.getDE()),           // sbc hl,de
        0x62 => self.sbcHL(self.getHL()),           // sbc hl,hl
        0x72 => self.sbcHL(self.sp),                // sbc hl,sp
        // rotate and shift group
        0x6f => self.rld(),                         // rld
        0x67 => self.rrd(),                         // rrd
        // call and return group
        0x4d => self.reti(),                        // reti
        0x45 => self.retn(),                        // retn
        0x55 => self.retn(),                        // retn
        0x5d => self.retn(),                        // retn
        0x65 => self.retn(),                        // retn
        0x6d => self.retn(),                        // retn
        0x75 => self.retn(),                        // retn
        0x7d => self.retn(),                        // retn
        // input and output group
        0x78 => self.inAFromC(),                    // in   a,(c)
        0x40 => self.setB(self.inFromC()),          // in   b,(c)
        0x48 => self.setC(self.inFromC()),          // in   c,(c)
        0x50 => self.setD(self.inFromC()),          // in   d,(c)
        0x58 => self.setE(self.inFromC()),          // in   e,(c)
        0x60 => self.setH(self.inFromC()),          // in   h,(c)
        0x68 => self.setL(self.inFromC()),          // in   l,(c)
        0x70 => _ = self.inFromC(),                 // in   (c)
        0xa2 => self.ini(),                         // ini
        0xb2 => self.inir(),                        // inir
        0xaa => self.ind(),                         // ind
        0xba => self.indr(),                        // indr
        0x79 => self.outAToC(),                     // out  (c),a
        0x41 => self.outToC(self.getB()),           // out  (c),b
        0x49 => self.outToC(self.getC()),           // out  (c),c
        0x51 => self.outToC(self.getD()),           // out  (c),d
        0x59 => self.outToC(self.getE()),           // out  (c),e
        0x61 => self.outToC(self.getH()),           // out  (c),h
        0x69 => self.outToC(self.getL()),           // out  (c),l
        0x71 => self.outToC(0),                     // out  (c),0
        0xa3 => self.outi(),                        // outi
        0xb3 => self.otir(),                        // otir
        0xab => self.outd(),                        // outd
        0xbb => self.otdr(),                        // otdr
        // remaining opcodes are considered nops
        else => {},
    }
}

/// Attempt to perform a maskable interrupt. Returns true if the interrupt was acknowledged.
pub fn irq(self: *CPU) bool {
    // request ignored if interrupts disabled or if not yet ready
    if (self.int_delay or !self.iff1) return false;
    // set up for response
    self.unhalt();
    self.di();
    self.irBug();
    // choose action depending on mode
    switch (self.im) {
        .Mode0 => {
            // read action from data bus
            self.addCycles(2);
            self.int_read = true;
        },
        .Mode1 => {
            // rst 38h
            self.addCycles(13);
            self.refresh();
            self.call(0x38);
        },
        .Mode2 => {
            self.addCycles(19);
            self.refresh();
            const indirect = word(self.i, self.interface.irq());
            const address = self.readWord(indirect);
            self.call(address);
        },
    }
    // acknowledge interrupt
    return true;
}

/// Attempt to perform a non-maskable interrupt. Returns true if the interrupt was acknowledged.
pub fn nmi(self: *CPU) bool {
    // request ignored if not yet ready
    if (self.int_delay) return false;
    // set up for response
    self.unhalt();
    self.irBug();
    self.refresh();
    self.addCycles(11);
    self.iff1 = false;
    // call nmi handler
    self.call(0x66);
    // acknowledge interrupt
    return true;
}

/// Performs a reset. This will only perform defined reset operations, and will take three cycles.
pub fn reset(self: *CPU) void {
    self.iff1 = false;
    self.iff2 = false;
    self.im = .Mode0;
    self.pc = 0;
    self.i = 0;
    self.r = 0;
    self.sp = 0xffff;
    self.setAF(0xffff);
    self.addCycles(3);
}
