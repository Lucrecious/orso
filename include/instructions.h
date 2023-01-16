#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

#include "type.h"
#include "chunk.h"
#include "opcodes.h"

#define ORSO_u24_to_u8s(BIG, A, B, C)\
    A = (byte)((BIG >> 16) & 0xFF);\
    B = (byte)((BIG >> 8) & 0xFF);\
    C = (byte)((BIG) & 0xFF)

#define ORSO_u8s_to_u24(A, B, C) (A << 16) | (B << 8) | (C)

#define ORSO_TypeKind_to_u8s(KIND, A, B)\
    A = (byte)((KIND >> 8) & 0xFF);\
    B = (byte)((KIND) & 0xFF)

#define ORSO_u8s_to_TypeKind(A, B) (A << 8) | (B)

#define ORSO_u64_to_u8s(TYPE_u64, A, B, C, D, E, F, G, H)\
    A = (byte)((TYPE_u64 >> 56) & 0xFF);\
    B = (byte)((TYPE_u64 >> 48) & 0xFF);\
    C = (byte)((TYPE_u64 >> 40) & 0xFF);\
    D = (byte)((TYPE_u64 >> 32) & 0xFF);\
    E = (byte)((TYPE_u64 >> 24) & 0xFF);\
    F = (byte)((TYPE_u64 >> 16) & 0xFF);\
    G = (byte)((TYPE_u64 >> 8) & 0xFF);\
    H = (byte)((TYPE_u64) & 0xFF)

#define ORSO_u8s_to_u64(A, B, C, D, E, F, G, H)\
    (A << 56) | (B << 48) | (C << 40) | (D << 32) |\
    (E << 24) | (F << 16) | (G << 8) | (H)

#endif