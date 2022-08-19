#ifndef LIST_H_
#define LIST_H_

#include "array.h"
#include "def.h"

typedef struct SavineList {
    i32 size;
    SavineArray items;
} SavineList;

void savine_list_new(SavineList* list, i32 element_size_bytes, i32 initial_capacity);
void savine_list_free(SavineList* list);

void savine_list_push_back(SavineList* list, void* item);


#endif