const std = @import("std");

pub fn from_fn(fun: anytype) Iter(FromFn(@TypeOf(fun))) {
    return .{ .impl = .{ .fun = fun } };
}

fn Iter(Impl: type) type {
    return struct {
        impl: Impl,

        fn for_each(self: @This(), fun: anytype) void {
            var impl = self.impl;
            while (impl.next()) |item| {
                _ = call_fn(fun, .{item});
            }
            impl.deinit();
        }
    };
}

fn Ret(Fn: type) type {
    const info = @typeInfo(Fn);
    if (info == .@"fn") return info.@"fn".return_type.?;
    if (@hasDecl(Fn, "call"))
        return @typeInfo(@TypeOf(@field(Fn, "call"))).@"fn".return_type.?;
    @compileError("Must be a function or callable");
}

fn call_fn(fun: anytype, args: anytype) Ret(@TypeOf(fun)) {
    if (@typeInfo(@TypeOf(fun)) == .@"fn") return @call(.auto, fun, args);
    return @call(.auto, @field(@TypeOf(fun), "call"), .{fun} ++ args);
}

fn deinit_fn(fun: anytype) void {
    if (@hasDecl(@TypeOf(fun), "deinit")) fun.deinit();
}

fn FromFn(Fn: type) type {
    return struct {
        fun: Fn,

        pub const Item = @typeInfo(Ret(Fn)).optional.child;

        pub fn next(self: *@This()) ?Item {
            return self.fun.call();
        }

        pub fn deinit(self: *@This()) void {
            deinit_fn(self.fun);
        }
    };
}
