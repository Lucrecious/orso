#include "list.h"

void savine_list_new(SavineList* list, i32 element_size_bytes, i32 initial_capacity) {
    savine_array_new(&list->items, element_size_bytes, initial_capacity);
    list->size = 0;
}

void savine_list_free(SavineList* list) {
    savine_array_free(&list->items);
}

void savine_list_push_back(SavineList* list, void* item) {
    if (list->size >= list->items.size) {
        SavineArray bigger_items;
        savine_array_new(&bigger_items, list->items.element_size_bytes, list->items.size * 2);

        for (i32 i = 0; i < list->items.size; i++) {
            bigger_items.items[i] = list->items.items[i];
        }

        savine_array_free(&list->items);
        list->items = bigger_items;
    }

    savine_array_insert(&list->items, list->size++, item);
}