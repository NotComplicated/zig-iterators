const std = @import("std");

pub fn from_fn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ .impl = .{ .fun = .{ .inner = fun } } };
}

pub fn from_slice(T: type, slice: []const T) Iter(Slice(T)) {
    return .{ .impl = .{ .slice = slice } };
}

fn Iter(Impl: type) type {
    return struct {
        impl: Impl,

        fn map(self: @This(), fun: anytype) Iter(Map(Impl, @TypeOf(fun))) {
            return .{
                .impl = .{
                    .inner = self.impl,
                    .fun = .{ .inner = fun },
                },
            };
        }

        fn for_each(self: @This(), fun: anytype) void {
            var impl = self.impl;
            var fun_: Fn(@TypeOf(fun)) = .{ .inner = fun };
            while (impl.next()) |item| {
                _ = fun_.call(.{item});
            }
            impl.deinit();
        }
    };
}

fn Fn(Inner: type) type {
    comptime var Deref = Inner;
    comptime var is_ptr = false;
    const inner_fn = inner_fn: switch (@typeInfo(Deref)) {
        .pointer => |ptr| {
            Deref = ptr.child;
            is_ptr = true;
            continue :inner_fn @typeInfo(Deref);
        },
        .@"fn" => |fn_| fn_,
        inline else => |other| {
            for (other.decls) |decl| {
                if (std.mem.eql(u8, decl.name, "call"))
                    break :inner_fn @typeInfo(@TypeOf(@field(Deref, "call"))).@"fn";
            }
            @compileError("Must be a function or have a `pub fn call(...)` method");
        },
    };

    return struct {
        inner: Inner,

        pub const Return = inner_fn.return_type.?;

        fn call(self: *@This(), args: anytype) Return {
            const deref: Deref = if (is_ptr) self.inner.* else self.inner;
            return switch (@typeInfo(Deref)) {
                .@"fn" => @call(.auto, deref, args),
                else => call: {
                    const self_param_is_ptr = @typeInfo(inner_fn.params[0].type.?) == .pointer;
                    const qualified = if (self_param_is_ptr and !is_ptr) &self.inner else self.inner;
                    break :call @call(.auto, @field(Deref, "call"), .{qualified} ++ args);
                },
            };
        }

        fn deinit(self: *@This()) void {
            if (@hasDecl(Deref, "deinit")) self.inner.deinit();
        }
    };
}

fn Map(Inner: type, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: Fn(FnInner),

        pub const Item = Fn(FnInner).Return;

        pub fn next(self: *@This()) ?Item {
            return self.fun.call(.{self.inner.next() orelse return null});
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.fun.deinit();
        }
    };
}

fn FromFn(FnInner: type) type {
    return struct {
        fun: Fn(FnInner),

        pub const Item = @typeInfo(Fn(FnInner).Return).optional.child;

        pub fn next(self: *@This()) ?Item {
            return self.fun.call(.{});
        }

        pub fn deinit(self: *@This()) void {
            self.fun.deinit();
        }
    };
}

fn Slice(T: type) type {
    return struct {
        slice: []const T,

        pub const Item = T;

        pub fn next(self: *@This()) ?Item {
            if (self.slice.len == 0) return null;
            const item = self.slice[0];
            self.slice = self.slice[1..];
            return item;
        }

        pub fn deinit(_: *@This()) void {}
    };
}

test "from_fn" {
    var collector = struct {
        nums: std.ArrayList(u32) = .empty,
        alloc: std.mem.Allocator = std.testing.allocator,
        pub fn call(self: *@This(), item: u32) void {
            self.nums.append(self.alloc, item) catch {};
        }
    }{};
    defer collector.nums.deinit(std.testing.allocator);

    from_fn(
        struct {
            i: u32 = 0,
            pub fn call(self: *@This()) ?u32 {
                if (self.i == 4) return null;
                self.i += 1;
                return self.i - 1;
            }
        }{},
    )
        .for_each(&collector);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2, 3 }, collector.nums.items);
    collector.nums.clearAndFree(std.testing.allocator);

    const fibs = struct {
        nums: std.ArrayList(u32) = .empty,
        alloc: std.mem.Allocator = std.testing.allocator,
        pub fn call(self: *@This()) ?u32 {
            if (self.nums.items.len == 0) self.nums.appendSlice(self.alloc, &.{ 0, 1 }) catch {};
            if (self.nums.items.len == 10) return null;
            const new = self.nums.getLast() + self.nums.items[self.nums.items.len - 2];
            self.nums.append(self.alloc, new) catch {};
            return new;
        }
        pub fn deinit(self: *@This()) void {
            self.nums.deinit(self.alloc);
        }
    }{};

    from_fn(fibs).for_each(&collector);
    try std.testing.expectEqualSlices(u32, &.{ 1, 2, 3, 5, 8, 13, 21, 34 }, collector.nums.items);
}
