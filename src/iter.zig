const std = @import("std");

pub fn fromFn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ ._impl = .{ .fun = .init(fun) } };
}

pub fn fromSlice(T: type, slice: []const T) Iter(Slice(T)) {
    return .{ ._impl = .{ .slice = slice } };
}

pub fn range(Int: type, from: Int, to: ?Int) Iter(Range(Int)) {
    return .{ .n = from, .to = to };
}

fn Iter(Impl: type) type {
    return struct {
        _impl: Impl,

        pub const Item = Impl.Item;

        const Self = @This();

        pub fn next(self: *Self) ?Item {
            return self._impl.next();
        }

        pub fn advanceBy(self: *Self, n: usize) error{Exhausted}!void {
            for (0..n) |_| self._impl.next() orelse return error.Exhausted;
        }

        pub fn nth(self: *Self, n: usize) ?Item {
            var item = null;
            for (0..n) |_| item = self._impl.next() orelse return null;
            return item;
        }

        pub fn map(self: Self, fun: anytype) Iter(Map(Impl, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn filter(self: Self, fun: anytype) Iter(Filter(Impl, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn filterMap(self: Self, fun: anytype) Iter(FilterMap(Impl, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn enumerate(self: Self, initial: usize) Iter(Enumerate(Impl)) {
            return .{ ._impl = .{ .inner = self._impl, .n = initial } };
        }

        pub fn stepBy(self: Self, step: usize) Iter(StepBy(Impl)) {
            if (step == 0) @panic("step must be > 0");
            return .{ ._impl = .{ .inner = self._impl, .step = step } };
        }

        pub fn chain(self: Self, other: anytype) Iter(Chain(Impl, @TypeOf(other))) {
            return .{ ._impl = .{ .first = self._impl, .second = other._impl } };
        }

        pub fn zip(self: Self, other: anytype) Iter(Zip(Impl, @TypeOf(other))) {
            return .{ ._impl = .{ .first = self._impl, .second = other._impl } };
        }

        pub fn consume(self: Self) void {
            var impl = self._impl;
            defer impl.deinit();
            while (impl.next()) |_| {}
        }

        pub fn forEach(self: Self, fun: anytype) void {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            while (impl.next()) |item| _ = fun_.call(.{item});
        }

        pub fn intoList(self: Self, alloc: std.mem.Allocator) std.mem.Allocator.Error!std.ArrayList(Item) {
            var list: std.ArrayList(Impl.Item) = try .initCapacity(alloc, self._impl.sizeHint()[0]);
            errdefer list.deinit(alloc);
            var impl = self._impl;
            defer impl.deinit();
            while (impl.next()) |item| try list.append(alloc, item);
            return list;
        }

        pub fn count(self: Self) usize {
            var impl = self._impl;
            defer impl.deinit();
            var total: usize = 0;
            while (impl.next()) |_| total += 1;
            return total;
        }

        pub fn last(self: Self) ?Item {
            var impl = self._impl;
            defer impl.deinit();
            var last_item = null;
            while (impl.next()) |item| last_item = item;
            return last_item;
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

fn Range(Int: type) type {
    return struct {
        n: Int,
        to: ?Int,

        pub const Item = Int;

        pub fn next(self: *@This()) ?Item {
            const n = self.n;
            if (self.to) |to| switch (std.math.order(n, to)) {
                .eq => return null,
                .lt => {},
                .gt => {
                    self.n -= 1;
                    return n;
                },
            };
            self.n += 1;
            return n;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return if (self.to) |to| switch (std.math.order(self.n, to)) {
                .eq => .{ 0, 0 },
                .lt => .{ @intCast(to - self.n), @intCast(to - self.n) },
                .gt => .{ @intCast(self.n - to), @intCast(self.n - to) },
            } else .{ @intCast(std.math.maxInt(Int) - self.n), null };
        }

        pub fn deinit(_: *@This()) void {}
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

fn Filter(Inner: type, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: Fn(FnInner),

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            while (self.inner.next()) |item| if (self.fun.call(.{item})) return item;
            return null;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ 0, self.inner.sizeHint()[1] };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.fun.deinit();
        }
    };
}

fn FilterMap(Inner: type, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: Fn(FnInner),

        pub const Item = @typeInfo(Fn(FnInner).Return).optional.child;

        pub fn next(self: *@This()) ?Item {
            while (self.inner.next()) |item| if (self.fun.call(.{item})) |mapped| return mapped;
            return null;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ 0, self.inner.sizeHint()[1] };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.fun.deinit();
        }
    };
}

fn Enumerate(Inner: type) type {
    return struct {
        inner: Inner,
        n: usize,

        pub const Item = struct { usize, Inner.Item };

        pub fn next(self: *@This()) ?Item {
            const item = .{ self.n, self.inner.next() orelse return null };
            self.n += 1;
            return item;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.inner.sizeHint();
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn StepBy(Inner: type) type {
    return struct {
        inner: Inner,
        step: usize,

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            const item = self.inner.next() orelse return null;
            for (0..self.step - 1) |_| self.inner.next() orelse break;
            return item;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const lower, const upper = self.inner.sizeHint();
            return .{ lower / self.step, if (upper) |u| u / self.step + 1 else null };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn Chain(First: type, Second: type) type {
    if (First.Item != Second.Item)
        @compileError("First and second iterator must produce the same item type");

    return struct {
        first: ?First,
        second: Second,

        pub const Item = First.Item;

        pub fn next(self: *@This()) ?Item {
            if (&self.first) |*first| {
                if (first.next()) |item| return item;
                first.* = null;
            } else return self.second.next();
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const first_lower, const first_upper = self.first.sizeHint();
            const second_lower, const second_upper = self.second.sizeHint();
            return .{ first_lower + second_lower, first_upper + second_upper };
        }

        pub fn deinit(self: *@This()) void {
            self.first.deinit();
            self.second.deinit();
        }
    };
}

fn Zip(First: type, Second: type) type {
    return struct {
        first: First,
        second: Second,

        pub const Item = struct { First.Item, Second.Item };

        pub fn next(self: *@This()) ?Item {
            return .{
                self.first.next() orelse return null,
                self.second.next() orelse return null,
            };
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const first_lower, const first_upper = self.first.sizeHint();
            const second_lower, const second_upper = self.second.sizeHint();
            return .{ @min(first_lower, second_lower), @min(first_upper, second_upper) };
        }

        pub fn deinit(self: *@This()) void {
            self.first.deinit();
            self.second.deinit();
        }
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
