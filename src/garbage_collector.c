#include "garbage_collector.h"
#include "virtual_machine.h"
#include "sb.h"

#ifdef DEBUG_GC_PRINT
#include <stdio.h>
#endif

#define GREY 2

void list_clear(OrsoGCHeader* list) {
    list->next = list;
    list->previous = list;
}

void orso_gc_init(OrsoGarbageCollector* gc, OrsoVM* vm) {
    list_clear(&gc->list1);
    list_clear(&gc->list2);

    gc->from = &gc->list1;
    gc->to = &gc->list2;
    gc->iterator = gc->to;

    gc->state = ORSO_GC_STATE_IDLE;
    gc->white = 0;
    gc->vm = vm;
}

static FORCE_INLINE void set_next(OrsoGCHeader* object, OrsoGCHeader* next) {
    object->next = next;
}

static FORCE_INLINE OrsoGCHeader* next(OrsoGCHeader* object) {
    return object->next;
}

static FORCE_INLINE void set_previous(OrsoGCHeader* object, OrsoGCHeader* previous) {
    object->previous = previous;
}

static FORCE_INLINE OrsoGCHeader* previous(OrsoGCHeader* object) {
    return object->previous;
}

static FORCE_INLINE void set_color(OrsoGCHeader* object, byte color) {
    object->color = color;
}

static FORCE_INLINE u32 color(OrsoGCHeader* object) {
    return object->color;
}

void list_push(OrsoGCHeader* list, OrsoGCHeader* object) {
    set_next(object, list);
    set_previous(object, list->previous);
    set_next(list->previous, object);
    set_previous(list, object);
} 

void orso_gc_register(OrsoGarbageCollector* gc, OrsoGCHeader* object) {
    list_push(gc->from, object);
    set_color(object, gc->white);
}

static void unlink(OrsoGCHeader* object) {
    OrsoGCHeader* previous_object = previous(object);
    OrsoGCHeader* next_object = next(object);

    set_previous(next_object, previous_object);
    set_next(previous_object, next_object);
}

static void grey(OrsoGarbageCollector* gc, OrsoGCHeader* object) {
    if (object == gc->iterator) {
        gc->iterator = previous(object);
    }

    unlink(object);
    list_push(gc->to, object);
    set_color(object, GREY);
}

static void visit(OrsoGarbageCollector* gc, OrsoGCHeader* object) {
    if (color(object) == gc->white) {
        grey(gc, object);
    }
}

static void mark_roots(OrsoGarbageCollector* gc) {
    OrsoVM* vm = gc->vm;
    for (OrsoGCValueIndex* index = vm->object_stack; index < vm->object_stack_top; index++) {
        if (!index->is_object) {
            continue;
        }

        OrsoObject* object = (OrsoObject*)vm->stack[index->index].as.p;
        /*
         * This needs be to be checked on the stack beacuse of blocks.
         * Local variables can temporarily be of an object type but point to a null stack slot. This happens
         * in blocks when creating a temporary variable for holding the result of the final expression.
         * In all other cases, the gc value index should point to an object is is_object is true.
        */
        if (object == NULL) {
            continue;
        }

        visit(gc, (OrsoGCHeader*)object);
    }

    for (i32 i = 0; i < vm->globals.name_to_index.capacity; i++) {
        OrsoSymbolTableEntry* entry = &vm->globals.name_to_index.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        visit(gc, (OrsoGCHeader*)entry->key);
    }

    for (i32 i = 0; i < sb_count(vm->globals.gc_values_indices); i++) {
        OrsoGCValueIndex index = vm->globals.gc_values_indices[i];
        if (!index.is_object) {
            continue;
        }


        visit(gc, (OrsoGCHeader*)vm->globals.values[index.index].as.p);
    }

    if (vm->chunk) {
        u32* constant_offsets = vm->chunk->constant_object_offsets;
        for (i32 i = 0; i < sb_count(constant_offsets); i++) {
            u32 offset = constant_offsets[i];
            OrsoSlot* slot = &vm->chunk->constants[offset];
            visit(gc, (OrsoGCHeader*)slot->as.p);
        }
    }
}

void orso_gc_step(OrsoGarbageCollector* gc) {
    switch (gc->state) {
        case ORSO_GC_STATE_IDLE: {
#ifdef DEBUG_GC_PRINT
            printf("-- gc begin\n");
#endif
            mark_roots(gc);
            gc->state = ORSO_GC_STATE_MARKING;
            break;
        }
        case ORSO_GC_STATE_MARKING: {
            OrsoGCHeader* object = next(gc->iterator);
            byte white = gc->white;
            if (object != gc->to) {
                gc->iterator = object;
                set_color(gc->iterator, !white);

                //mark_reachable(gc, object);
            } else {
                mark_roots(gc);

                object = next(gc->iterator);
                if (object == gc->to) {
                    /*
                        It's possible for objects to be created during garbage collection, which means during sweeping,
                        more objects can be added into the `from` list. However, the `from` list at this point holds unreachable
                        objects marked as white. Adding objects into this list will mix up processed objects with unprocessed
                        objects. The solution Bach Le uses is to swap the lists and flip the white color representation.
                        As a result of this the `from` list contains all previously reachable objects, and their `color`s
                        are marked white since the white was flipped (they are previous black in the `to` list).
                        Now, if more objects get added to the `from` list, they are all considered white which is good for the
                        next garbage collection cycle. Since we are sweeping, the `from` list is not touched. And lastly, this
                        makes unmarking objects O(1). No need to go through the reachable list to mark them back to white.
                    */ 
                    OrsoGCHeader* from = gc->from;
                    gc->from = gc->to;
                    gc->to = from;
                    gc->white = !white;
                    gc->iterator = next(from);
                    gc->state = ORSO_GC_STATE_SWEEPING;
                }
            }
            break;
        }
        case ORSO_GC_STATE_SWEEPING: {
            OrsoObject* object = (OrsoObject*)gc->iterator;
            if ((OrsoGCHeader*)object != gc->to) {
                gc->iterator = next((OrsoGCHeader*)object);

                switch (object->type_kind) {
                    case ORSO_TYPE_SYMBOL: orso_symbol_table_remove(&gc->vm->symbols, (OrsoSymbol*)object); break;
                    default: break; // Fast if I guess
                }
                orso_object_free(gc, object);
            } else {
                list_clear(gc->to);
                gc->state = ORSO_GC_STATE_IDLE;

#ifdef DEBUG_GC_PRINT
                printf("-- gc end\n");
#endif
            }
            break;
        }
    }
}

void orso_gc_collect(OrsoGarbageCollector* gc) {

    if (gc->state == ORSO_GC_STATE_IDLE) {
        orso_gc_step(gc);
    }

    while (gc->state != ORSO_GC_STATE_IDLE) {
        orso_gc_step(gc);
    }
}

#undef GREY
