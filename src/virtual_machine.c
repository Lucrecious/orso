#include "virtual_machine.h"

#include <stdio.h>

#include "codegen.h"
#include "instructions.h"
#include "opcodes.h"
#include "parser.h"
#include "static_analyzer.h"
#include "type.h"
#include "type_set.h"
#include "tmp.h"

#if defined(DEBUG)
#include "debug.h"
#endif

#include <time.h>

void vm_init(vm_t *vm, write_function_t write_fn, i32 stack_size) {
    vm->allocator = (arena_t){0};

    vm->frame_count = 0;

    vm->type_set = NULL;

    i32 stack_size_slot_count = bytes_to_slots(stack_size);

    vm->stack = arena_alloc(&vm->allocator, sizeof(slot_t)*stack_size_slot_count);
    vm->stack_top = vm->stack;

    vm->globals.values = (slots_t){.allocator=&vm->allocator};

    vm->stack_types_slot_count = 0;
    vm->stack_types = (types_t){.allocator = &vm->allocator};

    vm->globals.types = (types_t){.allocator = &vm->allocator};

    vm->write_fn = write_fn;

    symbol_table_init(&vm->symbols, &vm->allocator);
    symbol_table_init(&vm->globals.name_to_index, &vm->allocator);
}

void vm_free(vm_t* vm) {
    arena_free(&vm->allocator);
    vm->stack = NULL;
    vm->stack_top = NULL;

    vm->stack_types.count = 0;
    vm->globals.types.count = 0;

    vm->globals.values.count = 0;
}

static FORCE_INLINE void push_i64(vm_t* vm, slot_t value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

static FORCE_INLINE void reserve_stack_space(vm_t* vm, u32 slot_count) {
    for (u32 i = 0; i < slot_count; i++) {
        vm->stack_top->as.i = 0;
        vm->stack_top++;
    }
}

static void pop_type_n(vm_t *vm, size_t amount) {
    amount = amount < vm->stack_types.count ? amount : vm->stack_types.count;
    for (size_t i = vm->stack_types.count - amount; i < vm->stack_types.count; ++i) {
        type_info_t *stack_type = get_type_info(&vm->type_set->types, vm->stack_types.items[i]);
        vm->stack_types_slot_count -= type_slot_count(stack_type);
    }
    vm->stack_types.count -= amount;
}

static void push_type(vm_t *vm, type_t type_id) {
    array_push(&vm->stack_types, type_id);
    type_info_t *type = get_type_info(&vm->type_set->types, type_id);
    vm->stack_types_slot_count += type_slot_count(type);
}

void vm_push_object(vm_t* vm, object_t* object) {
    *vm->stack_top = SLOT_P(object);
    ++vm->stack_top;

    push_type(vm, object->type_id);
}

static FORCE_INLINE slot_t pop(vm_t* vm) {
    --vm->stack_top;
    return *vm->stack_top;
}

static FORCE_INLINE void pop_n(vm_t* vm, u32 count) {
    vm->stack_top -= count;
}

static FORCE_INLINE slot_t* peek(vm_t* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void call(vm_t* vm, function_t* function, i32 argument_slots) {
    if (vm->frame_count == FRAMES_MAX) {
        // TODO
        UNREACHABLE();
    }

    call_frame_t* frame = &vm->frames[vm->frame_count++];
    frame->function = function;
    frame->ip = function->chunk.code.items;
    frame->slots = vm->stack_top - argument_slots - 1;
}

void vm_call(vm_t *vm, function_t *function) {
    i32 argument_slots = 0;
    type_info_t *function_type = get_type_info(&vm->type_set->types, function->signature);
    for (size_t i = 0; i < function_type->data.function.argument_types.count; ++i) {
        type_info_t *arg_type = get_type_info(&vm->type_set->types, function_type->data.function.argument_types.items[i]);
        argument_slots += type_slot_count(arg_type);
    }
    call(vm, function, argument_slots);
}

static void call_object(vm_t *vm, object_t *callee, i32 argument_slots) {
    if (type_is_function(vm->type_set->types, callee->type_id)) {
        function_t *function = (function_t*)callee;
        call(vm, function, argument_slots);
        return;
    } else if (type_is_native_function(vm->type_set->types, callee->type_id)) {
        native_function_t *function_obj = (native_function_t*)callee;
        native_function_interface_t function = function_obj->function;
        function(vm->stack_top - argument_slots, vm->stack_top);

        type_info_t *function_type = get_type_info(&vm->type_set->types, function_obj->signature);
        type_info_t *return_type = get_type_info(&vm->type_set->types, function_type->data.function.return_type);

        i32 return_slot_size = type_slot_count(return_type);
        for (i32 i = 0; i < return_slot_size; i++) {
            vm->stack_top[-(argument_slots + 1) + i] = vm->stack_top[i];
        }

        // arguments on the stack + function, but remove the amount used for the return items
        vm->stack_top -= (argument_slots + 1) - return_slot_size;
        return;
    }

    UNREACHABLE();
}

void vm_print_stack(vm_t *vm) {
    i64 untracked_slots = 0;
    size_t effective_stack_type_count = vm->stack_types.count;
    size_t slot_count = (size_t)(vm->stack_top - vm->stack);
    if (vm->stack_types_slot_count > slot_count) {
        for (size_t i = vm->stack_types.count-1; i != 0; --i) {
            type_info_t *stack_type = get_type_info(&vm->type_set->types, vm->stack_types.items[i]);
            untracked_slots -= type_slot_count(stack_type);
            --effective_stack_type_count;
            if (vm->stack_types_slot_count + untracked_slots <= slot_count) {
                break;
            }
        }

        ASSERT(vm->stack_types_slot_count + untracked_slots == slot_count, "must be exact");
    } else {
        untracked_slots = (vm->stack_top - vm->stack) - vm->stack_types_slot_count;
    }

    static const size_t limit = 10;
    
    size_t items_counted = 0;
    size_t slots_size = 0;
    type_t types[limit] = {0};
    slot_t *slots[limit] = {0};

    size_t amount = limit;
    size_t offset = untracked_slots > 0 ? untracked_slots : 0;
    if (effective_stack_type_count < amount) amount = effective_stack_type_count;
    for (size_t i_ = 0; i_ < amount; ++i_) {
        size_t i = effective_stack_type_count - i_ - 1;
        type_t type_id = vm->stack_types.items[i];
        type_info_t *type = get_type_info(&vm->type_set->types, type_id);
        size_t size_slots = type_slot_count(type);

        slots_size += size_slots;
        slot_t *value_slot = vm->stack_top - slots_size - offset;
        slots[items_counted] = value_slot;
        types[items_counted] = type_id;
        ++items_counted;
    }

    tmp_arena_t *tmp = allocator_borrow(); {
        if (vm->stack_types.count > effective_stack_type_count) {
            printf("Unaccounted Types: ");
            for (size_t i = effective_stack_type_count; i < vm->stack_types.count; ++i) {
                type_t type_id = vm->stack_types.items[i];
                string_t type_str = type_to_string(vm->type_set->types, type_id, tmp->allocator);
                printf("%s", type_str.cstr);
                if (i < vm->stack_types.count-1) {
                    printf(",");
                }
            }

            printf("\n");
        }

        printf("Types Counted: %lu, Slots Showing: %lu, Untracked Slots: %lld\n", items_counted, slots_size, untracked_slots);
        for (size_t i = 0; i < items_counted; ++i) {
            slot_t *slot = slots[i];
            type_t type_id = types[i];
            size_t distance = slot - vm->stack;
            string_t value_string = slot_to_string(slot, &vm->type_set->types, type_id, tmp->allocator);
            printf("@%lu -> %s: %s\n", distance, value_string.cstr, type_to_string(vm->type_set->types, type_id, tmp->allocator).cstr);
        }
    } allocator_return(tmp);

}

void vm_disassemble_current_instruction(vm_t *vm) {
    call_frame_t *frame = &vm->frames[vm->frame_count-1];
    disassemble_instruction(&vm->type_set->types, &frame->function->chunk, frame->ip - frame->function->chunk.code.items);
}

bool vm_is_on_debug_instruction(vm_t *vm) {
    call_frame_t *frame = &vm->frames[vm->frame_count-1];
    size_t offset = frame->ip - frame->function->chunk.code.items;
    op_code_t op = frame->function->chunk.code.items[offset];
    switch (op) {
        case OP_PUSH_TYPE:
        case OP_POP_TYPE_N: {
            return true;
        }
        default: break;
    }
    return false;
}

void vm_begin(vm_t *vm, function_t *entry_point) {
    vm_push_object(vm, (object_t*)entry_point);
    vm_call(vm, entry_point);
}

#define READ_BYTE() *(frame->ip++)
#define READ_CODE(type) ((frame->ip += sizeof(type)-1, (type*)(frame->ip-sizeof(type))))
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POPN(N) pop_n(vm, N)
#define PUSH(VALUE) push_i64(vm, VALUE)

bool vm_step(vm_t *vm) {
    call_frame_t* frame = &vm->frames[vm->frame_count - 1];

    op_code_t op_code = READ_BYTE();
    switch (op_code) {
        case OP_NO_OP: break;

        case OP_POP: POP(); break;

        case OP_I64_TO_F64: *PEEK(0) = SLOT_F((f64)PEEK(0)->as.i); break;
        case OP_F64_TO_I64: *PEEK(0) = SLOT_I((i64)PEEK(0)->as.f); break;

        case OP_ADD_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i + b; break; }
        case OP_SUBTRACT_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i - b; break; }
        case OP_MULTIPLY_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i * b; break; }
        case OP_DIVIDE_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i / b; break; }

        case OP_ADD_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f + b; break; }
        case OP_SUBTRACT_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f - b; break; }
        case OP_MULTIPLY_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f * b; break; }
        case OP_DIVIDE_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f / b; break; }

        case OP_NEGATE_I64: PEEK(0)->as.i = -PEEK(0)->as.i; break;
        case OP_NEGATE_F64: PEEK(0)->as.f = -PEEK(0)->as.f; break;

        case OP_EQUAL_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i == b); break; }
        case OP_LESS_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i < b); break; }
        case OP_GREATER_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i > b); break; }

        case OP_EQUAL_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f == b); break; }
        case OP_LESS_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f < b); break; }
        case OP_GREATER_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f > b); break; }

        case OP_ADD_PTR_I64: {
            i64 b = POP().as.i;
            PEEK(0)->as.p = (void*)(((byte*)PEEK(0)->as.p) + b);
            break;
        }

        case OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->as.p == PEEK(0)->as.p); POP(); POP(); PUSH(SLOT_I(result)); break; }

        case OP_EQUAL_STRING: { i32 result = string_equal(PEEK(1)->as.p, PEEK(0)->as.p); POP(); POP(); PUSH(SLOT_I(result)); break; }
        case OP_CONCAT_STRING: { PEEK(1)->as.p = orso_string_concat(PEEK(1)->as.p, PEEK(0)->as.p, &vm->allocator); POP(); break; } 

        case OP_LOGICAL_NOT: PEEK(0)->as.i = !PEEK(0)->as.i; break;

        case OP_PUSH_0: {
            const slot_t zero = SLOT_I(0);
            PUSH(zero);
            break;
        }
        case OP_PUSH_1: {
            const slot_t one = SLOT_I(1);
            PUSH(one);
            break;
        }

        case OP_PUSH_LOCAL_ADDRESS: {
            op_push_address_t *push_address = READ_CODE(op_push_address_t);
            u16 index = push_address->index;
            PUSH(SLOT_P(((byte*)frame->slots) + index));
            break;
        }

        case OP_PUSH_GLOBAL_ADDRESS: {
            op_push_address_t *push_address = READ_CODE(op_push_address_t);
            u16 index = push_address->index;
            PUSH(SLOT_P(((byte*)vm->globals.values.items) + index));
            break;
        }

        case OP_POP_SCOPE: {
            op_pop_scope_t *pop_scope = READ_CODE(op_pop_scope_t);
            u32 stack_size = vm->stack_top - vm->stack;

            for (byte i = 0; i < pop_scope->value_size_slots; ++i) {
                vm->stack[stack_size - (pop_scope->scope_size_slots + pop_scope->value_size_slots) + i] = *(vm->stack_top - pop_scope->value_size_slots + i);
            }

            POPN(pop_scope->scope_size_slots);
            break;
        }

        case OP_POPN: {
            op_popn_t *popn = READ_CODE(op_popn_t);
            POPN(popn->n);
            break;
        }

        case OP_CONSTANT: {
            op_location_t *location = READ_CODE(op_location_t);
            u32 index = location->index_slots;
            u16 size = location->size_bytes;

            slot_t* current_top = vm->stack_top;
            reserve_stack_space(vm, bytes_to_slots(size));
            byte* constant = ((byte*)frame->function->chunk.constants.items) + index;
            memcpy(current_top, constant, size);
            break;
        }

        case OP_GLOBAL: {
            op_location_t *location = READ_CODE(op_location_t);
            u32 index = location->index_slots;
            u16 size = location->size_bytes;

            slot_t* current_top = vm->stack_top;
            reserve_stack_space(vm, bytes_to_slots(size));
            byte* global = ((byte*)vm->globals.values.items) + index;
            memcpy(current_top, global, size);
            break;
        }

        case OP_LOCAL: {
            op_location_t *location = READ_CODE(op_location_t);
            u32 index = location->index_slots;
            u16 size = location->size_bytes;

            slot_t* current_top = vm->stack_top;
            reserve_stack_space(vm, bytes_to_slots(size));
            byte* local = ((byte*)frame->slots) + index;
            memcpy(current_top, local, size);
            break;
        }

        case OP_FIELD: {
            op_field_t *field = READ_CODE(op_field_t);
            u16 field_offset = field->offset_bytes;
            type_kind_t type_kind = field->type_kind;
            u16 field_size = field->size_bytes;
            u16 value_size = field->value_size_bytes;
            ASSERT(value_size % sizeof(slot_t) == 0, "must be aligned to slots");

            byte *start = ((byte*)vm->stack_top) - bytes_to_slots(value_size)*sizeof(slot_t);

            copy_bytes_to_slots(start, start+field_offset, type_kind, field_size);

            POPN(bytes_to_slots(value_size) - bytes_to_slots(field_size));
            break;
        }

        case OP_SET_LVALUE: {
            op_set_lvalue_t *set_lvalue = READ_CODE(op_set_lvalue_t);
            type_kind_t type_kind = set_lvalue->type_kind;
            void* ptr = POP().as.p;
            switch (type_kind) {
                case TYPE_UNION:
                case TYPE_STRUCT: {
                    byte size = set_lvalue->size_bytes;
                    byte slots = bytes_to_slots(size);
                    memcpy(ptr, PEEK(slots - 1), size);
                    break;
                }

                case TYPE_FLOAT32: {
                    f32 value  = (f32)PEEK(0)->as.f;
                    *((f32*)ptr) = value;
                    break;
                }

                case TYPE_INT32: {
                    i32 value = (i32)PEEK(0)->as.i;
                    *((i32*)ptr) = value;
                    break;
                }

                case TYPE_VOID:
                case TYPE_BOOL: {
                    byte value = (byte)PEEK(0)->as.u;
                    *((byte*)ptr) = value;
                    break;
                }

                case TYPE_TYPE:
                case TYPE_FLOAT64:
                case TYPE_INT64:
                case TYPE_NATIVE_FUNCTION:
                case TYPE_FUNCTION:
                case TYPE_SYMBOL:
                case TYPE_STRING:
                case TYPE_POINTER: {
                    slot_t value = *PEEK(0);
                    *((slot_t*)ptr) = value;
                    break;
                }

                case TYPE_COUNT:
                case TYPE_INVALID:
                case TYPE_UNDEFINED:
                case TYPE_UNRESOLVED: {
                    UNREACHABLE();
                    break;
                }
            }
            break;
        }

        case OP_PUT_IN_UNION: {
            op_put_in_union_t *put_in_union = READ_CODE(op_put_in_union_t);
            byte size = put_in_union->size_bytes;

            type_info_t* type = (type_info_t*)(PEEK(0)->as.p);
            byte slot_size = bytes_to_slots(size);
            memmove(PEEK(slot_size-1), PEEK(slot_size), slot_size * sizeof(slot_t));
            PEEK(slot_size)->as.p = type;
            break;
        }

        case OP_NARROW_UNION: {
            op_narrow_union_t *narrow_union = READ_CODE(op_narrow_union_t);
            byte type_offset = narrow_union->offset_bytes;
            ASSERT(type_offset % sizeof(slot_t), "must be perfectly slot size");
            byte slot_offset = type_offset / sizeof(slot_t);

            memmove(PEEK(slot_offset - 1), PEEK(slot_offset - 2), type_offset - sizeof(slot_t));

            POP();
            break;
        }

        case OP_JUMP_IF_UNION_FALSE: {
            UNREACHABLE();
            break;
        }

        case OP_JUMP_IF_FALSE: {
            op_jump_t *jump = READ_CODE(op_jump_t);
            if (SLOT_IS_FALSEy(*PEEK(0))) {
                frame->ip += jump->offset;
            }
            break;
        }

        case OP_JUMP_IF_UNION_TRUE: {
            UNREACHABLE();
            break;
        }
        case OP_JUMP_IF_TRUE: {
            op_jump_t *jump = READ_CODE(op_jump_t);
            if (!SLOT_IS_FALSEy(*PEEK(0))) {
                frame->ip += jump->offset;
            }
            break;
        }

        case OP_JUMP: {
            op_jump_t *jump = READ_CODE(op_jump_t);
            frame->ip += jump->offset;
            break;
        }

        case OP_LOOP: {
            op_loop_t *loop = READ_CODE(op_loop_t);
            u16 offset = loop->offset;
            frame->ip -= offset;
            break;
        }

        case OP_CALL: {
            op_call_t *call = READ_CODE(op_call_t);
            u16 argument_slots = call->argument_slots;
            call_object(vm, (object_t*)PEEK(argument_slots)->as.p, argument_slots);

            frame = &vm->frames[vm->frame_count - 1];
            break;
        }

        case OP_PRINT:
        case OP_PRINT_EXPR: {
            type_t type_id = (type_t){.i = POP().as.u}; // pop expression type
            type_info_t *type = get_type_info(&vm->type_set->types, type_id);

            OrsoString *expression_string = (OrsoString*)(POP().as.p);

            byte size_slots = type_slot_count(type);
            tmp_arena_t *tmp = allocator_borrow(); {
                OrsoString *value_string = orso_slot_to_string(PEEK(size_slots - 1), &vm->type_set->types, type_id, tmp->allocator);

                if (vm->write_fn != NULL) {
                    if (op_code == OP_PRINT_EXPR) {
                        vm->write_fn(expression_string->text);
                        vm->write_fn(" (");

                            string_t s = type_to_string(vm->type_set->types, type_id, tmp->allocator);
                            vm->write_fn(s.cstr);
                        vm->write_fn(") => ");
                        vm->write_fn(value_string->text);
                        vm->write_fn("\n");
                    } else {
                        vm->write_fn(value_string->text);
                        vm->write_fn("\n");
                    }
                }
            } allocator_return(tmp);

            POPN(size_slots);
            break;
        }
        
        case OP_RETURN: {
            op_return_t *return_ = READ_CODE(op_return_t);

            byte result_size = return_->size_slots;
            for (i32 i = 0; i < result_size; ++i) {
                frame->slots[i] = *PEEK(result_size - i - 1);
            }

            --vm->frame_count;

            if (vm->frame_count == 0) {
                return false;
            }

            vm->stack_top = frame->slots + result_size;
            frame = &vm->frames[vm->frame_count - 1];
            break;
        }
        case OP_POP_TYPE_N: {
            op_push_pop_type_t *push_pop_type = READ_CODE(op_push_pop_type_t);
            pop_type_n(vm, push_pop_type->data.n);
            break;
        }
        
        case OP_PUSH_TYPE: {
            op_push_pop_type_t *push_pop_type = READ_CODE(op_push_pop_type_t);
            push_type(vm, push_pop_type->data.type_id);
            break;
        }
    }

    return true;
}

static void run(vm_t *vm, error_function_t error_fn) {
    (void)error_fn;
     while (vm_step(vm));
}

#undef PUSH
#undef READ_CODE
#undef POPN
#undef POP
#undef PEEK
#undef READ_BYTE


void vm_interpret(vm_t* vm, error_function_t error_fn) {
    run(vm, error_fn);
}
