X(clock_ns, clock_ns, u64, {
    UNUSED(args);
    XRET(u64, clock_ns);
}, void)

X(ns2sec, ns2sec, f64, {
    XARG(ns, u64);
    XRET(f64, ns2sec, ns);
}, u64)

X(add, add_, u8, {
    XARG(b, u8);
    XARG(a, u8);
    XRET(u8, add_, a, b);
}, u8, u8)