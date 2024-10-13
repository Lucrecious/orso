#include "object.h"

#include <stdio.h>

#include "sb.h"
#include "symbol_table.h"
#include "type_set.h"

void* orso_object_reallocate(OrsoObject* pointer, type_t* type, size_t old_size, size_t new_size) {
    (void)old_size;

    if (new_size == 0) {
        // Should only be here when called by GC
        free(pointer);
        return NULL;
    }

    OrsoObject* result = realloc(pointer, new_size);
    if (result == NULL) {
        exit(1);
    }

    result->type = type;

    return result;
}

void orso_object_free(OrsoObject* object) {
#ifdef DEBUG_GC_PRINT
    char type_buffer[256];
    orso_type_to_cstrn(object->type, type_buffer, 256);
    printf("%p free type %s\n", (void*)object, type_buffer);
#endif
    switch (object->type->kind) {
        case ORSO_TYPE_SYMBOL: {
            OrsoSymbol* symbol = (OrsoSymbol*)object;
            orso_object_reallocate((OrsoObject*)symbol, &OrsoTypeSymbol, sizeof(OrsoSymbol) + symbol->length, 0);
            break;
        }
        case ORSO_TYPE_STRING: {
            OrsoString* string = (OrsoString*)object;
            orso_object_reallocate((OrsoObject*)string, &OrsoTypeString, sizeof(OrsoString) + string->length, 0);
            break;
        }
        case ORSO_TYPE_FUNCTION: {
            function_t* function = (function_t*)object;
            chunk_free(&function->chunk);
            orso_object_reallocate((OrsoObject*)function, (type_t*)&OrsoTypeEmptyFunction, sizeof(function_t), 0);
            break;
        }
        default: UNREACHABLE();
    }
}

OrsoString* orso_new_string_from_cstrn(const char* start, i32 length) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(OrsoString, &OrsoTypeString, length + 1);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

char* cstrn_new(const char* start, i32 length) {
    char* cstr = ORSO_ALLOCATE_N(char, length + 1);
    memcpy(cstr, start, length);
    cstr[length] = '\0';

    return cstr;
}

char* orso_slot_to_new_cstrn(slot_t* slot, type_t* type) {
    switch (type->kind) {
        case ORSO_TYPE_BOOL: {
            if (ORSO_SLOT_IS_FALSE((*slot))) {
                return cstrn_new("false", 5);
            } else {
                return cstrn_new("true", 4);
            }
        }

        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: {
            // Max characters for i64 is 19 + sign and \0
            char buffer[21];
            i32 length = snprintf(buffer, 21, "%lld", slot->as.i);
            return cstrn_new(buffer, length);
        }

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: {
            // Using Wren's num to string for this: https://github.com/wren-lang/wren/blob/main/src/vm/wren_value.c#L775
            char buffer[24];
            i32 length;
            if (slot->as.f == (i64)slot->as.f) {
                length = snprintf(buffer, 24, "%.1f", slot->as.f);
            } else {
                length = snprintf(buffer, 24, "%.14g", slot->as.f);
            }
            return cstrn_new(buffer, length);
        }

        case ORSO_TYPE_VOID: return cstrn_new("null", 4);

        case ORSO_TYPE_STRING: return cstrn_new(((OrsoString*)slot->as.p)->text, ((OrsoString*)slot->as.p)->length);

        case ORSO_TYPE_SYMBOL: {
            OrsoSymbol* symbol = (OrsoSymbol*)slot->as.p;
            // 2 single quotes
            // 1 \0
            char buffer[symbol->length + 3];
            snprintf(buffer, symbol->length + 3, "'%s'", symbol->text);
            return cstrn_new(buffer, symbol->length + 3);
        }

        case ORSO_TYPE_FUNCTION: {
            function_t* function = (function_t*)slot->as.p;
            const i32 BUFFER_SIZE = 500;
            char buffer[BUFFER_SIZE];
            i32 n = snprintf(buffer, BUFFER_SIZE, "<%s :: (",
                    function->binded_name ? function->binded_name->text : "<anonymous>");

            char tmp_buffer[128];
            for (i32 i = 0; i < function->signature->data.function.argument_count; i++) {
                orso_type_to_cstrn(function->signature->data.function.argument_types[i], tmp_buffer, 128);

                n += snprintf(buffer + n, BUFFER_SIZE - n, "%s%s", tmp_buffer,
                        i == function->signature->data.function.argument_count - 1 ? "" : ", ");
            }

            orso_type_to_cstrn(function->signature->data.function.return_type, tmp_buffer, 128);
            n += snprintf(buffer + n, BUFFER_SIZE - n, ") -> %s>", tmp_buffer);
            
            buffer[n] = '\0';

            return cstrn_new(buffer, n);
        }

        case ORSO_TYPE_NATIVE_FUNCTION: {
            return cstrn_new("<native>", 8);
        }

        case ORSO_TYPE_POINTER: {
            return cstrn_new("<ptr>", 5);
        }

        case ORSO_TYPE_STRUCT: {
            u32 size = type->data.struct_.total_bytes;
            char buffer[256];
            snprintf(buffer, 256, "<struct %d bytes>", size);
            return cstrn_new(buffer, strlen(buffer));
        }

        case ORSO_TYPE_UNION: {
            type_t* type = (type_t*)slot->as.p;
            char* cstr = orso_slot_to_new_cstrn(slot + 1, type);
            char* union_cstr = "%s";
            size_t buffer_length = strlen(cstr) + strlen(union_cstr);
            char buffer[buffer_length + 1];
            snprintf(buffer, buffer_length + 1, union_cstr, cstr);
            free(cstr);
            return cstrn_new(buffer, buffer_length);
        }

        case ORSO_TYPE_TYPE: {
            type_t* type = (type_t*)slot->as.p;

            char buffer[128];

            char tmp_buffer[128];
            orso_type_to_cstrn(type, tmp_buffer, 128);

            snprintf(buffer, 128, "<%s>", tmp_buffer);

            return cstrn_new(buffer, strlen(buffer));
        }
        case ORSO_TYPE_UNDEFINED: return cstrn_new("<undefined>", 11);
        case ORSO_TYPE_UNRESOLVED: return cstrn_new("<unresolved>", 12);
        case ORSO_TYPE_INVALID: return cstrn_new("<invalid>", 9);
    }
}

OrsoString* orso_slot_to_string(slot_t* slot, type_t* type) {
    char* cstr = orso_slot_to_new_cstrn(slot, type);
    OrsoString* string = orso_new_string_from_cstrn(cstr, strlen(cstr));
    free(cstr);

    return string;
}

OrsoString* orso_string_concat(OrsoString* a, OrsoString* b) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(OrsoString, &OrsoTypeString, a->length + b->length + 1);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

function_t* orso_new_function(void) {
    function_t* function = ORSO_OBJECT_ALLOCATE(function_t, (type_t*)&OrsoTypeEmptyFunction);
    function->signature = &OrsoTypeEmptyFunction;
    chunk_init(&function->chunk);
    function->binded_name = NULL;

    return function;
}

bool is_function_compiled(function_t* function) {
    return function->chunk.code != NULL;
}

OrsoNativeFunction* orso_new_native_function(NativeFunction function, type_t* type) {
    OrsoNativeFunction* function_obj = ORSO_OBJECT_ALLOCATE(OrsoNativeFunction, type);
    function_obj->function = function;
    function_obj->signature = type;

    return function_obj;
}

OrsoStruct* orso_new_struct(void) {
    OrsoStruct* struct_ = ORSO_ALLOCATE(OrsoStruct);
    struct_->slots = NULL;
    return struct_;
}

void orso_free_struct(OrsoStruct* struct_) {
    free(struct_->slots);
}

i64 cstrn_to_i64(const char* text, i32 length) {
    i64 integer = 0;

    for (i32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        i32 ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            continue;
        }

        integer *= 10;
        integer += ones;
    }

    return integer;
}

f64 cstrn_to_f64(const char* text, i32 length) {
    f64 value = 0;
    f64 fact = 1;
    bool point_seen = false;

    for (char* c = (char*)text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        i32 digit = *c - '0';
        if (digit < 0 || digit > 9) {
            continue;
        }

        if (point_seen) {
            fact /= 10.0f;
        }

        value = value * 10.0f + (float)digit;
    }

    return value * fact;
}

OrsoSymbol* orso_unmanaged_symbol_from_cstrn(const char* start, i32 length, symbol_table_t* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_ALLOCATE_FLEX(OrsoSymbol, length + 1);
    symbol->object.type = &OrsoTypeSymbol;
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = ORSO_SLOT_I(0);
    orso_symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}

OrsoSymbol* orso_new_symbol_from_cstrn(const char* start, i32 length, symbol_table_t* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_OBJECT_ALLOCATE_FLEX(OrsoSymbol, &OrsoTypeSymbol, length + 1);
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = ORSO_SLOT_I(0);
    orso_symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}
