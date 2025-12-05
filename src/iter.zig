const std = @import("std");

pub fn fromFn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ ._impl = .{ .fun = .init(fun) } };
}

pub fn fromSlice(T: type, slice: []const T) Iter(Slice(T)) {
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

        fn forEach(self: @This(), fun: anytype) void {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            while (impl.next()) |item| _ = fun_.call(.{item});
        }

        fn intoList(self: @This(), alloc: std.mem.Allocator) !std.ArrayList(Impl.Item) {
            var list: std.ArrayList(Impl.Item) = try .initCapacity(alloc, self._impl.sizeHint()[0]);
            errdefer list.deinit(alloc);
            var impl = self._impl;
            defer impl.deinit();
            while (impl.next()) |item| try list.append(alloc, item);
            return list;
        }

        fn count(self: @This()) usize {
            var impl = self._impl;
            defer impl.deinit();
            var total: usize = 0;
            while (impl.next()) |_| total += 1;
            return total;
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

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.inner.sizeHint();
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

        pub fn sizeHint(_: @This()) struct { usize, ?usize } {
            return .{ 0, null };
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

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ self.slice.len, self.slice.len };
        }

        pub fn deinit(_: *@This()) void {}
    };
}

test "fromFn" {
    var collect = struct {
        nums: std.ArrayList(u32) = .empty,
        alloc: std.mem.Allocator = std.testing.allocator,
        pub fn call(self: *@This(), item: u32) void {
            self.nums.append(self.alloc, item) catch {};
        }
    }{};
    defer collect.nums.deinit(std.testing.allocator);

    fromFn(
        struct {
            i: u32 = 0,
            pub fn call(self: *@This()) ?u32 {
                if (self.i == 4) return null;
                self.i += 1;
                return self.i - 1;
            }
        }{},
    )
        .forEach(&collect);
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

    fromFn(fibs).forEach(&collect);
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

    fromSlice(u8, "foobar")
        .map(std.ascii.toUpper)
        .forEach(&collect);

    try std.testing.expectEqualSlices(u8, "FOOBAR", collect.chars.items);
}

test "intoList" {
    var list = try fromSlice(bool, &.{ true, false, false, true })
        .map(
            struct {
                fn f(b: bool) bool {
                    return !b;
                }
            }.f,
        )
        .intoList(std.testing.allocator);
    defer list.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(bool, &.{ false, true, true, false }, list.items);
}

test "count" {
    try std.testing.expectEqual(9, fromSlice(u8, "foobarbaz").count());
}
