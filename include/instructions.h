#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

#include "type.h"
#include "chunk.h"
#include "opcodes.h"

#define u24_to_u8s(BIG, A, B, C)\
    A = (byte)((BIG >> 16) & 0xFF);\
    B = (byte)((BIG >> 8) & 0xFF);\
    C = (byte)((BIG) & 0xFF)

#define u32_to_u8s(BIG, A, B, C, D)\
    A = (byte)((BIG >> 24) & 0xFF);\
    B = (byte)((BIG >> 16) & 0xFF);\
    C = (byte)((BIG >> 8) & 0xFF);\
    D = (byte)((BIG) & 0xFF)

#define u8s_to_u24(A, B, C) (A << 16) | (B << 8) | (C)

#define u16_to_u8s(BIG, A, B)\
    A = (byte)((BIG >> 8) & 0xFF);\
    B = (byte)((BIG) & 0xFF)

#define u8s_to_u16(A, B) (A << 8) | (B)

#define u8s_to_u32(A, B, C, D) (((u32)(A<<24)) | ((u32)(B<<16)) | ((u32)(C<<8)) | (D))

#define type_kind_to_u8s(KIND, A, B)\
    A = (byte)((KIND >> 8) & 0xFF);\
    B = (byte)((KIND) & 0xFF)

#define u8s_to_type_kind(A, B) (A << 8) | (B)

#define u64_to_u8s(TYPE_u64, A, B, C, D, E, F, G, H)\
    A = (byte)((TYPE_u64 >> 56) & 0xFF);\
    B = (byte)((TYPE_u64 >> 48) & 0xFF);\
    C = (byte)((TYPE_u64 >> 40) & 0xFF);\
    D = (byte)((TYPE_u64 >> 32) & 0xFF);\
    E = (byte)((TYPE_u64 >> 24) & 0xFF);\
    F = (byte)((TYPE_u64 >> 16) & 0xFF);\
    G = (byte)((TYPE_u64 >> 8) & 0xFF);\
    H = (byte)((TYPE_u64) & 0xFF)

#define u8s_to_u64(A, B, C, D, E, F, G, H)\
    ((u64)A << 56) | ((u64)B << 48) | ((u64)C << 40) | ((u64)D << 32) |\
    ((u32)E << 24) | ((u32)F << 16) | ((u16)G << 8) | ((byte)H)

#endif
