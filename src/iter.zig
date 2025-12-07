const std = @import("std");

pub fn fromFn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ ._impl = .{ .fun = .init(fun) } };
}

pub fn fromSlice(T: type, slice: []const T) Iter(Slice(T)) {
    return .{ ._impl = .{ .slice = slice } };
}

pub fn fromSliceMut(T: type, slice: []T) Iter(SliceMut(T)) {
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

        pub fn peek(self: *Self) ?Item {
            if (!std.meta.hasMethod(Impl, "peek")) @compileError("Must call 'peekable' before peeking");
            return self._impl.peek();
        }

        pub fn advanceBy(self: *Self, n: usize) error{Exhausted}!void {
            for (0..n) |_| self._impl.next() orelse return error.Exhausted;
        }

        pub fn nth(self: *Self, n: usize) ?Item {
            var item = null;
            for (0..n) |_| item = self._impl.next() orelse return null;
            return item;
        }

        pub fn byRef(self: *Self) Iter(ByRef(Impl)) {
            return .{ ._impl = .{ .inner = &self._impl } };
        }

        pub fn clone(self: Self) !Iter(Impl) {
            return .{ ._impl = try self._impl.clone() };
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

        pub fn flatten(self: Self) Iter(Flatten(Impl)) {
            return .{ ._impl = .{ .inner = self._impl } };
        }

        pub fn flattenMonadic(self: Self) Iter(FlattenMonadic(Impl)) {
            return .{ ._impl = .{ .inner = self._impl } };
        }

        pub fn flattenIndexed(self: Self) Iter(FlattenIndexed(Impl)) {
            return .{ ._impl = .{ .inner = self._impl } };
        }

        pub fn mapWindows(self: Self, comptime size: usize, fun: anytype) Iter(MapWindows(Impl, size, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn skip(self: Self, n: usize) Iter(Impl) {
            var impl = self._impl;
            for (0..n) |_| impl.next();
            return .{ ._impl = impl };
        }

        pub fn take(self: Self, n: usize) Iter(Take(Impl)) {
            return .{ ._impl = .{ .inner = self._impl, .n = n } };
        }

        pub fn skipWhile(self: Self, fun: anytype) Iter(SkipWhile(Impl, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn takeWhile(self: Self, fun: anytype) Iter(TakeWhile(Impl, @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .fun = .init(fun) } };
        }

        pub fn enumerate(self: Self, initial: usize) Iter(Enumerate(Impl)) {
            return .{ ._impl = .{ .inner = self._impl, .n = initial } };
        }

        pub fn stepBy(self: Self, step: usize) Iter(StepBy(Impl)) {
            if (step == 0) @panic("step must be > 0");
            return .{ ._impl = .{ .inner = self._impl, .step = step } };
        }

        pub fn chunk(self: Self, comptime size: usize) Iter(Chunk(Impl, size)) {
            return .{ ._impl = .{ .inner = self._impl } };
        }

        pub fn chain(self: Self, other: anytype) Iter(Chain(Impl, @TypeOf(other))) {
            return .{ ._impl = .{ .first = self._impl, .second = other._impl } };
        }

        pub fn zip(self: Self, other: anytype) Iter(Zip(Impl, @TypeOf(other))) {
            return .{ ._impl = .{ .first = self._impl, .second = other._impl } };
        }

        pub fn index(self: Self, i: usize) Iter(Index(Impl, i)) {
            return .{ ._impl = .{ .inner = self._impl } };
        }

        pub fn cycle(self: Self) !Iter(Cycle(Impl)) {
            return .{ ._impl = .{ .inner = self._impl, .benched = try self._impl.clone() } };
        }

        pub fn scan(self: Self, state: anytype, fun: anytype) Iter(Scan(Impl, @TypeOf(state), @TypeOf(fun))) {
            return .{ ._impl = .{ .inner = self._impl, .state = state, .fun = .init(fun) } };
        }

        pub fn dyn(self: Self, alloc: std.mem.Allocator) std.mem.Allocator.Error!Iter(Dyn(Item)) {
            return .{ ._impl = try .init(self._impl, alloc) };
        }

        pub fn dynRef(self: Self) Iter(DynRef(Item)) {
            return .{ ._impl = .init(Impl, self._impl) };
        }

        pub fn peekable(self: Self) Iter(Peek(Impl)) {
            return .{ ._impl = .{ .inner = self._impl } };
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
            defer fun_.deinit();
            while (impl.next()) |item| _ = fun_.call(.{item});
        }

        pub fn intoList(self: Self, alloc: std.mem.Allocator) std.mem.Allocator.Error!std.ArrayList(Item) {
            var list: std.ArrayList(Item) = try .initCapacity(alloc, self._impl.sizeHint()[0]);
            errdefer list.deinit(alloc);
            var impl = self._impl;
            defer impl.deinit();
            while (impl.next()) |item| try list.append(alloc, item);
            return list;
        }

        pub fn intoHashMap(self: Self, alloc: std.mem.Allocator) std.mem.Allocator.Error!std.AutoHashMapUnmanaged(Item[0], Item[1]) {
            var hash_map: std.AutoHashMapUnmanaged(Item[0], Item[1]) = .empty;
            errdefer hash_map.deinit(alloc);
            hash_map.ensureTotalCapacity(alloc, self._impl.sizeHint()[0]);
            var impl = self._impl;
            defer impl.deinit();
            while (impl.next()) |item| try hash_map.put(alloc, item[0], item[1]);
            return hash_map;
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

        pub fn fold(self: Self, initial: anytype, fun: anytype) @TypeOf(initial) {
            var impl = self._impl;
            defer impl.deinit();
            const FoldFn = Fn(@TypeOf(fun));
            var fun_: FoldFn = .init(fun);
            defer fun_.deinit();
            if (FoldFn.Return != @TypeOf(initial)) @compileError("Fold function returns wrong type");
            var folded = initial;
            while (impl.next()) |item| {
                folded = fun_.call(.{ folded, item });
            }
            return folded;
        }

        pub fn reduce(self: Self, fun: anytype) ?Item {
            var self_ = self;
            const initial = self_.next() orelse {
                self_._impl.deinit();
                return null;
            };
            return self_.fold(initial, fun);
        }

        pub fn all(self: Self, fun: anytype) bool {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            defer fun_.deinit();
            while (impl.next()) |item| if (!fun_.call(.{item})) return false;
            return true;
        }

        pub fn any(self: Self, fun: anytype) bool {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            defer fun_.deinit();
            while (impl.next()) |item| if (fun_.call(.{item})) return true;
            return false;
        }

        pub fn find(self: Self, fun: anytype) ?Item {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            defer fun_.deinit();
            while (impl.next()) |item| if (fun_.call(.{item})) return item;
            return null;
        }

        pub fn position(self: Self, fun: anytype) ?usize {
            var impl = self._impl;
            defer impl.deinit();
            var fun_: Fn(@TypeOf(fun)) = .init(fun);
            defer fun_.deinit();
            var pos = 0;
            while (impl.next()) |item| : (pos += 1) if (fun_.call(.{item})) return pos;
            return null;
        }

        pub fn max(self: Self) ?Item {
            var impl = self._impl;
            defer impl.deinit();
            var max_item = impl.next() orelse return null;
            while (impl.next()) |item| max_item = @max(item, max_item);
            return max_item;
        }

        pub fn min(self: Self) ?Item {
            var impl = self._impl;
            defer impl.deinit();
            var min_item = impl.next() orelse return null;
            while (impl.next()) |item| min_item = @min(item, min_item);
            return min_item;
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

        pub fn clone(self: @This()) !@This() {
            return if (std.meta.hasMethod(TrueInner, "clone")) .{ .inner = try self.inner.clone() } else self;
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

        pub fn clone(self: @This()) !@This() {
            return .{ .fun = try self.fun.clone() };
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

        pub fn clone(self: @This()) !@This() {
            return self;
        }

        pub fn deinit(_: *@This()) void {}
    };
}

fn SliceMut(T: type) type {
    return struct {
        slice: []T,

        pub const Item = *T;

        pub fn next(self: *@This()) ?Item {
            if (self.slice.len == 0) return null;
            const item = &self.slice[0];
            self.slice = self.slice[1..];
            return item;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ self.slice.len, self.slice.len };
        }

        pub fn clone(self: @This()) !@This() {
            return self;
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

        pub fn clone(self: @This()) !@This() {
            return self;
        }

        pub fn deinit(_: *@This()) void {}
    };
}

fn ByRef(Inner: type) type {
    return struct {
        inner: *Inner,

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            return self.inner.next();
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.inner.sizeHint();
        }

        pub fn clone(self: @This()) !@This() {
            return self;
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

        pub fn clone(self: @This()) !@This() {
            const inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = try self.fun.clone() };
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

        pub fn clone(self: @This()) !@This() {
            const inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = try self.fun.clone() };
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

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = try self.fun.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.fun.deinit();
        }
    };
}

fn Flatten(Inner: type) type {
    if (!@hasDecl(Inner, "Item")) @compileError("Flatten can only be called on an iterator of iterators");

    return struct {
        inner: Inner,
        flattening: ?Inner.Item = null,

        pub const Item = Inner.Item.Item;

        pub fn next(self: *@This()) ?Item {
            while (true) {
                if (&self.flattening) |*iter|
                    return iter.next() orelse {
                        self.flattening = null;
                        continue;
                    }
                else
                    self.flattening = self.inner.next() orelse return null;
            }
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ if (self.flattening) |iter| iter.sizeHint()[0] else 0, null };
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .flattening = if (self.flattening) |iter| try iter.clone() else null };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            if (&self.flattening) |*iter| iter.deinit();
        }
    };
}

fn FlattenMonadic(Inner: type) type {
    return struct {
        inner: Inner,

        pub const Item = switch (@typeInfo(Inner.Item)) {
            .optional => |opt| opt.child,
            .error_union => |eu| eu.payload,
            else => @compileError("Must call flattenMonadic with an optional or error union item type"),
        };

        pub fn next(self: *@This()) ?Item {
            while (true) {
                const item = self.inner.next() orelse return null;
                switch (@typeInfo(Inner.Item)) {
                    .optional => return item orelse continue,
                    .error_union => return item catch continue,
                    else => unreachable,
                }
            }
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ 0, self.inner.sizeHint()[1] };
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .inner = try self.inner.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn FlattenIndexed(Inner: type) type {
    return struct {
        inner: Inner,
        cached: ?Item = null,
        index: usize = 0,

        pub const Item = item: switch (@typeInfo(Inner.Item)) {
            .pointer => |ptr| switch (ptr.size) {
                .many, .slice => ptr.child,
                else => continue :item ptr.child,
            },
            .array => |arr| arr.child,
            .@"struct" => |st| if (st.is_tuple) {
                for (st.fields[1..]) |field|
                    if (field.type != st.fields[0].type) @compileError("All types in tuple must match");
                break :item st.fields[0].type;
            } else @compileError("Item is not indexable"),
            .vector => |v| v.child,
            else => @compileError("Item is not indexable"),
        };

        pub fn next(self: *@This()) ?Item {
            if (self.cached) |cached| if (self.index < cached.len) {
                self.index += 1;
                return cached[self.index - 1];
            };
            self.cached = self.inner.next() orelse return null;
            self.index = 0;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const cached_size = if (self.cached) |cached| cached.len - self.index else 0;
            switch (@typeInfo(Inner.Item)) {
                .pointer => return .{ cached_size, null },
                .vector, .array => |av| {
                    const lower, const upper = self.inner.sizeHint();
                    return .{ lower *| av.len +| cached_size, upper *| av.len +| cached_size };
                },
                .@"struct" => |st| {
                    const lower, const upper = self.inner.sizeHint();
                    return .{ lower *| st.fields.len +| cached_size, upper *| st.fields.len +| cached_size };
                },
                else => unreachable,
            }
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .inner = try self.inner.clone(), .cached = self.cached, .index = self.index };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn MapWindows(Inner: type, size: usize, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: Fn(FnInner),
        window: ?[size]Item,

        pub const Item = Fn(FnInner).Return;

        pub fn next(self: *@This()) ?Item {
            if (&self.window) |*window| {
                const item = self.inner.next() orelse return null;
                std.mem.rotate(Item, window, 1);
                window[window.len - 1] = item;
            } else {
                var window: [size]Item = undefined;
                for (&window) |*item| item.* = self.inner.next() orelse return null;
                self.window = window;
            }
            return self.fun.call(.{&self.window.?});
        }

        pub fn sizeHint(self: *@This()) struct { usize, ?usize } {
            const lower, const upper = self.inner.sizeHint();
            return if (self.window == null)
                .{ lower - size + 1, if (upper) |u| u - size + 1 else null }
            else
                .{ lower, upper };
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = try self.fun.clone(), .window = self.window };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.fun.deinit();
        }
    };
}

fn Take(Inner: type) type {
    return struct {
        inner: Inner,
        n: usize,

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            if (self.n == 0) return null;
            self.n -= 1;
            return self.inner.next();
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const lower, const upper = self.inner.sizeHint();
            return .{ @min(self.n, lower), @min(self.n, upper) };
        }

        pub fn clone(self: @This()) !@This() {
            return self;
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn SkipWhile(Inner: type, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: ?Fn(FnInner),

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            while (self.inner.next()) |item| {
                if (self.fun) |fun| {
                    if (!fun.call(.{item})) {
                        self.fun.?.deinit();
                        self.fun = null;
                    }
                } else return item;
            }
            return null;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ 0, self.inner.sizeHint()[1] };
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = if (self.fun) |fun| try fun.clone() else null };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            if (self.fun) |*fun| fun.deinit();
        }
    };
}
fn TakeWhile(Inner: type, FnInner: type) type {
    return struct {
        inner: Inner,
        fun: ?Fn(FnInner),

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            const item = self.inner.next() orelse return null;
            const fun = self.fun orelse return null;
            if (!fun.call(.{item})) {
                self.fun.?.deinit();
                self.fun = null;
                return null;
            }
            return item;
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return .{ 0, self.inner.sizeHint()[1] };
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .fun = if (self.fun) |fun| try fun.clone() else null };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            if (self.fun) |*fun| fun.deinit();
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

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .n = self.n };
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

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .step = self.step };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn Chunk(Inner: type, size: usize) type {
    return struct {
        inner: Inner,

        pub const Item = struct { [size]Item, usize };

        pub fn next(self: *@This()) ?Item {
            var chunk: [size]Item = undefined;
            for (&chunk, 0..) |*item, i| {
                item.* = self.inner.next() orelse return if (i == 0) null else .{ chunk, i };
            }
            return .{ chunk, size };
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const lower, const upper = self.inner.sizeHint();
            return .{ lower / size + 1, if (upper) |u| u / size + 1 else null };
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .inner = try self.inner.clone() };
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
            return .{ first_lower +| second_lower, first_upper +| second_upper };
        }

        pub fn clone(self: @This()) !@This() {
            var first = try self.first.clone();
            errdefer first.deinit();
            return .{ .first = first, .second = try self.second.clone() };
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

        pub fn clone(self: @This()) !@This() {
            var first = try self.first.clone();
            errdefer first.deinit();
            return .{ .first = first, .second = try self.second.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.first.deinit();
            self.second.deinit();
        }
    };
}

fn Index(Inner: type, i: usize) type {
    return struct {
        inner: Inner,

        pub const Item = item: switch (@typeInfo(Inner.Item)) {
            .pointer => |ptr| switch (ptr.size) {
                .many, .slice => ptr.child,
                else => continue :item ptr.child,
            },
            .array => |arr| if (arr.len > i) arr.child else @compileError("Index out of bounds"),
            .@"struct" => |st| if (st.is_tuple)
                if (st.fields.len > i) st.fields[i].type else @compileError("Index out of bounds")
            else
                @compileError("Item is not indexable"),
            .vector => |v| if (v.len > i) v.child else @compileError("Index out of bounds"),
            else => @compileError("Item is not indexable"),
        };

        pub fn next(self: *@This()) ?Item {
            return (self.inner.next() orelse return null)[i];
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.inner.sizeHint();
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .inner = try self.inner.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
        }
    };
}

fn Cycle(Inner: type) type {
    return struct {
        inner: Inner,
        benched: Inner,

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            return self.inner.next() orelse {
                self.inner.* = self.benched.clone() catch return null;
                return self.inner.next();
            };
        }

        pub fn sizeHint(_: @This()) struct { usize, ?usize } {
            return .{ std.math.maxInt(usize), null };
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            return .{ .inner = inner, .benched = try self.benched.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            self.benched.deinit();
        }
    };
}

fn Scan(Inner: type, State: type, FnInner: type) type {
    return struct {
        inner: Inner,
        state: State,
        fun: Fn(FnInner),

        pub const Item = @typeInfo(Fn(FnInner).Return).optional.child;

        pub fn next(self: *@This()) ?Item {
            return self.fun.call(.{ &self.state, self.inner.next() orelse return null });
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.inner.sizeHint();
        }

        pub fn clone(self: @This()) !@This() {
            var inner = try self.inner.clone();
            errdefer inner.deinit();
            const has_clone = std.meta.hasMethod(State, "clone");
            const has_deinit = std.meta.hasMethod(State, "deinit");
            var state = if (has_clone) try self.state.clone() else self.state;
            errdefer if (has_clone and has_deinit) state.deinit();
            return .{ .inner = inner, .state = state, .fun = try self.fun.clone() };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
            if (std.meta.hasMethod(State, "deinit")) self.state.deinit();
            self.fun.deinit();
        }
    };
}

fn Dyn(Item_: type) type {
    return struct {
        data: *anyopaque,
        vtable: *const VTable,

        pub const Item = Item_;

        const VTable = struct {
            next: *const fn (self: *anyopaque) ?Item,
            sizeHint: *const fn (self: *anyopaque) struct { usize, ?usize },
            clone: *const fn (self: *anyopaque) anyerror!*anyopaque,
            deinit: *const fn (self: *anyopaque) void,
        };

        pub fn init(inner: anytype, alloc: std.mem.Allocator) std.mem.Allocator.Error!Dyn(Item) {
            const Data = struct { @TypeOf(inner), std.mem.Allocator };
            const VTEntry = struct {
                fn next(self: *anyopaque) ?Item {
                    const data: *Data = @ptrCast(@alignCast(self));
                    return data[0].next();
                }
                fn sizeHint(self: *anyopaque) struct { usize, ?usize } {
                    const data: *Data = @ptrCast(@alignCast(self));
                    return data[0].sizeHint();
                }
                fn clone(self: *anyopaque) anyerror!*anyopaque {
                    const data: *Data = @ptrCast(@alignCast(self));
                    const cloned_data = try data[1].create(Data);
                    errdefer data[1].destroy(cloned_data);
                    cloned_data[0] = try data[0].clone();
                    cloned_data[1] = data[1];
                    return cloned_data;
                }
                fn deinit(self: *anyopaque) void {
                    const data: *Data = @ptrCast(@alignCast(self));
                    data[0].deinit();
                    data[1].destroy(data);
                }
                const vtable: VTable = .{
                    .next = @This().next,
                    .sizeHint = @This().sizeHint,
                    .deinit = @This().deinit,
                };
            };
            const data = try alloc.create(Data);
            data.* = .{ inner, alloc };
            return .{ .data = data, .vtable = &VTEntry.vtable };
        }

        pub fn next(self: *@This()) ?Item {
            return self.vtable.next(self.data);
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.vtable.sizeHint(self.data);
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .data = try self.vtable.clone(self.data), .vtable = self.vtable };
        }

        pub fn deinit(self: *@This()) void {
            self.vtable.deinit(self.data);
        }
    };
}

fn DynRef(Item_: type) type {
    return struct {
        data: *anyopaque,
        vtable: *const VTable,

        pub const Item = Item_;

        const VTable = struct {
            next: *const fn (self: *anyopaque) ?Item,
            sizeHint: *const fn (self: *anyopaque) struct { usize, ?usize },
        };

        pub fn init(Inner: type, inner: *Inner) Dyn(Item) {
            const VTEntry = struct {
                fn next(self: *anyopaque) ?Item {
                    const old_self: *Inner = @ptrCast(@alignCast(self));
                    return old_self.next();
                }
                fn sizeHint(self: *anyopaque) struct { usize, ?usize } {
                    const old_self: *Inner = @ptrCast(@alignCast(self));
                    return old_self.sizeHint();
                }
                const vtable: VTable = .{
                    .next = @This().next,
                    .sizeHint = @This().sizeHint,
                };
            };
            return .{ .data = inner, .vtable = &VTEntry.vtable };
        }

        pub fn next(self: *@This()) ?Item {
            return self.vtable.next(self.data);
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            return self.vtable.sizeHint(self.data);
        }

        pub fn clone(self: @This()) !@This() {
            return self;
        }

        pub fn deinit(_: *@This()) void {}
    };
}

fn Peek(Inner: type) type {
    return struct {
        inner: Inner,
        cached: ?Item,

        pub const Item = Inner.Item;

        pub fn next(self: *@This()) ?Item {
            if (self.cached) |cached| {
                self.cached = null;
                return cached;
            }
            return self.inner.next();
        }

        pub fn peek(self: *@This()) ?Item {
            return self.cached orelse {
                self.cached = self.inner.next();
                return self.cached;
            };
        }

        pub fn sizeHint(self: @This()) struct { usize, ?usize } {
            const added = if (self.cached == null) 0 else 1;
            const lower, const upper = self.inner.sizeHint();
            return .{ lower +| added, if (upper) |u| u +| added else null };
        }

        pub fn clone(self: @This()) !@This() {
            return .{ .inner = try self.inner.clone(), .cached = self.cached };
        }

        pub fn deinit(self: *@This()) void {
            self.inner.deinit();
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
