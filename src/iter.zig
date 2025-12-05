const std = @import("std");

pub fn from_fn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ ._impl = .{ .fun = .init(fun) } };
}

pub fn from_slice(T: type, slice: []const T) Iter(Slice(T)) {
    return .{ ._impl = .{ .slice = slice } };
}

fn Iter(Impl: type) type {
    return struct {
        _impl: Impl,

        fn map(self: @This(), fun: anytype) Iter(Map(Impl, @TypeOf(fun))) {
            return .{
                ._impl = .{
                    .inner = self._impl,
                    .fun = .init(fun),
                },
            };
        }

        fn for_each(self: @This(), fun: anytype) void {
            var impl = self._impl;
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            while (impl.next()) |item| {
                _ = fun_.call(.{item});
            }
            impl.deinit();
        }
    };
}

fn Fn(Inner: type) type {
    const is_comptime_fn = @typeInfo(Inner) == .@"fn";
    const TrueInner = if (is_comptime_fn) *const Inner else Inner;
    comptime var Deref = Inner;
    comptime var is_ptr = is_comptime_fn;
    const inner_fn = inner_fn: switch (@typeInfo(Deref)) {
        .pointer => |ptr| {
            Deref = ptr.child;
            is_ptr = true;
            continue :inner_fn @typeInfo(Deref);
        },
        .@"fn" => |fn_| fn_,
        else => if (std.meta.hasFn(Deref, "call"))
            @typeInfo(@TypeOf(@field(Deref, "call"))).@"fn"
        else
            @compileError("Must be a function or have a `pub fn call(...)` method"),
    };

    return struct {
        inner: TrueInner,

        pub const Return = inner_fn.return_type.?;

        pub fn init(inner: Inner) @This() {
            return .{ .inner = if (is_comptime_fn) &inner else inner };
        }

        pub fn call(self: *@This(), args: anytype) Return {
            return switch (@typeInfo(Deref)) {
                .@"fn" => @call(.auto, self.inner, args),
                else => call: {
                    const self_param_is_ptr = @typeInfo(inner_fn.params[0].type.?) == .pointer;
                    const qualified = if (self_param_is_ptr and !is_ptr) &self.inner else self.inner;
                    break :call @call(.auto, @field(Deref, "call"), .{qualified} ++ args);
                },
            };
        }

        pub fn deinit(self: *@This()) void {
            if (std.meta.hasMethod(TrueInner, "deinit")) self.inner.deinit();
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
    var collect = struct {
        nums: std.ArrayList(u32) = .empty,
        alloc: std.mem.Allocator = std.testing.allocator,
        pub fn call(self: *@This(), item: u32) void {
            self.nums.append(self.alloc, item) catch {};
        }
    }{};
    defer collect.nums.deinit(std.testing.allocator);

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
        .for_each(&collect);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2, 3 }, collect.nums.items);
    collect.nums.clearAndFree(std.testing.allocator);

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

    from_fn(fibs).for_each(&collect);
    try std.testing.expectEqualSlices(u32, &.{ 1, 2, 3, 5, 8, 13, 21, 34 }, collect.nums.items);
}

test "slice" {
    var collect: struct {
        chars: std.ArrayList(u8) = .empty,
        pub fn call(self: *@This(), c: u8) void {
            self.chars.append(std.testing.allocator, c) catch {};
        }
    } = .{};
    defer collect.chars.deinit(std.testing.allocator);

    from_slice(u8, "foobar")
        .map(std.ascii.toUpper)
        .for_each(&collect);

    try std.testing.expectEqualSlices(u8, "FOOBAR", collect.chars.items);
}
