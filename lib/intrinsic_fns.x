// X(export_fn_name, c_fn_name, return_type, args...)
// XARG(arg_name, type) - must be done in reverse order of parameter order
// XRET(return_type, c_fn_name, args...)

X(clock_ns, clock_ns, u64, {
    UNUSED(args);
    XRET(u64, clock_ns);
}, void)

//X(ns2sec, ns2sec, f64, {
//    XARG(ns, u64);
//    XRET(f64, ns2sec, ns);
//}, u64)

X(mreserve, odlmreserve, void*, {
    XARG(size, size_t);
    XRET(void*, odlmreserve, size);
}, size_t)

X(mmarkrw, odlmmarkrw, bool_, {
    XARG(size, size_t);
    XARG(addr, void*);
    XRET(bool_, odlmmarkrw, addr, size);
}, void*, size_t)

X(mmarkro, odlmmarkro, bool_, {
    XARG(size, size_t);
    XARG(addr, void*);
    XRET(bool_, odlmmarkro, addr, size);
}, void*, size_t)

X(mfree, odlmfree, bool_, {
    XARG(size, size_t);
    XARG(addr, void*);
    XRET(bool_, odlmfree, addr, size);
}, void*, size_t)

X(mpagesize, odlmpagesize, size_t, {
    UNUSED(args);
    XRET(size_t, odlmpagesize);
}, void)

X(readint, odlreadint, int, {
    UNUSED(args);
    XRET(int, odlreadint);
}, void)

X(printint, odlprintint, void, {
    XARG(i, int);
    UNUSED(result);
    odlprintint(i);
}, int)

X(printf64, odlprintf64, void, {
    XARG(i, f64);
    UNUSED(result);
    odlprintf64(i);
}, f64)

X(printu64, odlprintu64, void, {
    XARG(i, u64);
    UNUSED(result);
    odlprintu64(i);
}, u64)

X(prints64, odlprints64, void, {
    XARG(i, s64);
    UNUSED(result);
    odlprints64(i);
}, s64)

X(println, odlprintln, void, {
    UNUSED(args);
    UNUSED(result);
    odlprintln();
}, void)