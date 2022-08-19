#ifndef BYTE_CODE_H_
#define BYTE_CODE_H_

#include "abstract_syntax_tree.h"

typedef struct ByteCode {
    i32* code;
} ByteCode;

void generate_byte_code(struct ByteCode* byte_code, struct AbstractSyntaxTree* ast) { }

void byte_code_free(struct ByteCode* byte_code) { }

#endif