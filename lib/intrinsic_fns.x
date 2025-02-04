// X(export_fn_name, c_fn_name, return_type, args...)
// XARG(arg_name, type) - must be done in reverse order of parameter order
// XRET(return_type, c_fn_name, args...)

X(clock_ns, clock_ns, u64, {
    UNUSED(args);
    XRET(u64, clock_ns);
}, void)

X(ns2sec, ns2sec, f64, {
    XARG(ns, u64);
    XRET(f64, ns2sec, ns);
}, u64)