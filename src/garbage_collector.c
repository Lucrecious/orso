#include "garbage_collector.h"
#include "virtual_machine.h"

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

void orso_gc_step(OrsoGarbageCollector* gc) {
    switch (gc->state) {
        case ORSO_GC_STATE_IDLE: {
            //mark_from_roots(gc, gc->vm);
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
                //mark_from_roots(gc, gc->vm);

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
            OrsoGCHeader* object = gc->iterator;
            if (object != gc->to) {
                gc->iterator = next(object);
                ORSO_OBJECT_FREE(gc, object);
            } else {
                list_clear(gc->to);
                gc->state = ORSO_GC_STATE_IDLE;
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