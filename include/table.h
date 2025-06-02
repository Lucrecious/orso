#ifndef TABLE_H_
#define TABLE_H_

#include "khash.h"


typedef khiter_t tbliter_t;

#define protocol_table(NAME, K, V) \
    table_##NAME##_t *table_##NAME##_new(arena_t *allocator); \
    bool table_##NAME##_put(kh_##NAME##_t *h, K key, V value); \
    bool table_##NAME##_get(kh_##NAME##_t *h, K key, V *result); \
    void table_##NAME##_clear(kh_##NAME##_t *h);\
    tbliter_t table_##NAME##_begin(kh_##NAME##_t *h);\
    tbliter_t table_##NAME##_end(kh_##NAME##_t *h);


#define implement_table(NAME, K, V, HASH, EQUALS) \
    __KHASH_IMPL(NAME, , K, V, true, HASH, EQUALS) \
    table_##NAME##_t *table_##NAME##_new(arena_t *allocator) { \
        return kh_init(NAME, allocator); \
    } \
    bool table_##NAME##_put(kh_##NAME##_t *h, K key, V value) { \
        int r = 0; \
        khint_t index = kh_put(NAME, h, key, &r); \
        kh_val(h, index) = value; \
        return r == 0; \
    } \
    bool table_##NAME##_get(kh_##NAME##_t *h, K key, V *result) { \
        khint_t index = kh_get(NAME, h, key); \
        if (index == kh_end(h)) return false; \
        *result = kh_val(h, index); \
        return true; \
    } \
    void table_##NAME##_clear(kh_##NAME##_t *h) { \
        kh_clear(NAME, h); \
    }\
    tbliter_t table_##NAME##_begin(kh_##NAME##_t *h) {\
        ORUNUSED(h);\
        return  kh_begin(h);\
    }\
    tbliter_t table_##NAME##_end(kh_##NAME##_t *h) {\
        return  kh_end(h);\
    } 


#define define_table(NAME, K, V, HASH, EQUALS) \
    KHASH_DECLARE(NAME, K, V) \
    typedef kh_##NAME##_t table_##NAME##_t; \
    implement_table(NAME, K, V, HASH, EQUALS)

#define declare_table(NAME, K, V) \
    KHASH_DECLARE(NAME, K, V) \
    typedef kh_##NAME##_t table_##NAME##_t; \
    protocol_table(NAME, K, V)

#define table_new(NAME, allocator) table_##NAME##_new(allocator)
#define table_put(NAME, h, key, result) table_##NAME##_put(h, key, result)
#define table_get(NAME, h, key, result) table_##NAME##_get(h, key, result)
#define table_clear(NAME, h) table_##NAME##_clear(h)
#define table_begin(NAME, h) table_##NAME##_begin(h)
#define table_end(NAME, h) table_##NAME##_end(h)
#define table_t(NAME) table_##NAME##_t

#endif
