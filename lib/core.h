#ifndef CORE_H_
#define CORE_H_

#include "intrinsics.h"

#define X(export_fn_name, c_fn_name, return_type, code, ...) return_type c_fn_name(__VA_ARGS__);
#define XARG(name, type)
#define XRET(type, c_fn_name, ...)
#include "intrinsic_fns.x"
#undef XRET
#undef XARG
#undef X

#define X(export_fn_name, c_fn_name, return_type, ...) void c_fn_name##_i_(void*,void*);
#define XARG(name, type)
#define XRET(type, c_fn_name, ...)
#include "intrinsic_fns.x"
#undef XRET
#undef XARG
#undef X


#endif
