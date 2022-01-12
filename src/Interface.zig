const Interface = @This();

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    /// Read from memory.
    read: fn(ptr: *anyopaque, addr: u16) u8,
    /// Write to memory.
    write: fn(ptr: *anyopaque, addr: u16, value: u8) void,
    /// Read data for a maskable interrupt (address low byte for mode 2, opcode byte(s) for mode 0)
    irq: fn(ptr: *anyopaque) u8,
    /// Read from I/O.
    in: fn(ptr: *anyopaque, port: u16) u8,
    /// Write to I/O.
    out: fn(ptr: *anyopaque, port: u16, value: u8) void,
    /// Called when an interrupt routine is completed.
    reti: fn(ptr: *anyopaque) void,
};

fn writeDefault(ptr: *anyopaque, addr: u16, value: u8) void {
    _ = ptr;
    _ = addr;
    _ = value;
}

fn irqDefault(ptr: *anyopaque) u8 {
    _ = ptr;
    return 0;
}

fn inDefault(ptr: *anyopaque, port: u16) u8 {
    _ = ptr;
    _ = port;
    return 0;
}

fn outDefault(ptr: *anyopaque, port: u16, value: u8) void {
    _ = ptr;
    _ = port;
    _ = value;
}

fn retiDefault(ptr: *anyopaque) void {
    _ = ptr;
}

pub fn init(
    pointer: anytype,
    comptime vtable: struct {
        read: fn(self: @TypeOf(pointer), addr: u16) u8,
        write: ?fn(self: @TypeOf(pointer), addr: u16, value: u8) void = null,
        irq: ?fn(self: @TypeOf(pointer)) u8 = null,
        in: ?fn(self: @TypeOf(pointer), port: u16) u8 = null,
        out: ?fn(self: @TypeOf(pointer), port: u16, value: u8) void = null,
        reti: ?fn(self: @TypeOf(pointer)) void = null,
    },
) Interface {
    const Ptr = @TypeOf(pointer);
    const alignment = @alignOf(@TypeOf(pointer.*));

    const gen = struct {
        inline fn cast(ptr: *anyopaque) Ptr {
            return @ptrCast(Ptr, @alignCast(alignment, ptr));
        }

        fn readImpl(ptr: *anyopaque, addr: u16) u8 {
            return vtable.read(cast(ptr), addr);
        }

        fn writeImpl(ptr: *anyopaque, addr: u16, value: u8) void {
            return vtable.write.?(cast(ptr), addr, value);
        }

        fn irqImpl(ptr: *anyopaque) u8 {
            return vtable.irq.?(cast(ptr));
        }

        fn inImpl(ptr: *anyopaque, port: u16) u8 {
            return vtable.in.?(cast(ptr), port);
        }

        fn outImpl(ptr: *anyopaque, port: u16, value: u8) void {
            return vtable.out.?(cast(ptr), port, value);
        }

        fn retiImpl(ptr: *anyopaque) void {
            return vtable.reti.?(cast(ptr));
        }

        const gen_vtable = VTable{
            .read = readImpl,
            .write = if (vtable.write != null) writeImpl else writeDefault,
            .irq = if (vtable.irq != null) irqImpl else irqDefault,
            .in = if (vtable.in != null) inImpl else inDefault,
            .out = if (vtable.out != null) outImpl else outDefault,
            .reti = if (vtable.reti != null) retiImpl else retiDefault,
        };
    };

    return .{
        .ptr = pointer,
        .vtable = &gen.gen_vtable,
    };
}

pub inline fn read(self: Interface, addr: u16) u8 {
    return self.vtable.read(self.ptr, addr);
}

pub inline fn write(self: Interface, addr: u16, value: u8) void {
    return self.vtable.write(self.ptr, addr, value);
}

pub inline fn irq(self: Interface) u8 {
    return self.vtable.irq(self.ptr);
}

pub inline fn in(self: Interface, port: u16) u8 {
    return self.vtable.in(self.ptr, port);
}

pub inline fn out(self: Interface, port: u16, value: u8) void {
    return self.vtable.out(self.ptr, port, value);
}

pub inline fn reti(self: Interface) void {
    return self.vtable.reti(self.ptr);
}
