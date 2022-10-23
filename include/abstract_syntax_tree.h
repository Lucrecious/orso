#ifndef ABSTRACT_SYNTAX_TREE_H_
#define ABSTRACT_SYNTAX_TREE_H_

#include "lexer.h"
#include "type.h"
#include "value.h"

typedef enum ExpressionType {
    EXPRESSION_NONE,
    EXPRESSION_IMPLICIT_CAST,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_GROUPING,
    EXPRESSION_PRIMARY,
} ExpressionType;

typedef struct SavineExpressionNode SavineExpressionNode;

typedef struct SavineImplicitCastOp {
    SavineExpressionNode* operand;
} SavineImplicitCastOp;

typedef struct SavineBinaryOp {
    Token operator;
    SavineExpressionNode* left;
    SavineExpressionNode* right;
} SavineBinaryOp;

typedef struct SavineUnaryOp {
    Token operator;
    SavineExpressionNode* operand;
} SavineUnaryOp;

typedef struct SavineGrouping {
    SavineExpressionNode* expression;
} SavineGrouping;

typedef struct SavinePrimary {
    Token token;
    SavineValue constant;
} SavinePrimary;

struct SavineExpressionNode {
    SavineType value_type;
    ExpressionType type;
    union {
        SavineImplicitCastOp cast;
        SavineBinaryOp binary;
        SavineUnaryOp unary;
        SavineGrouping grouping;
        SavinePrimary primary;
    };
};

typedef struct SavineAST {
    SavineExpressionNode* expression;
} SavineAST;

void savine_ast_print(SavineAST* ast, const char* name);

bool savine_parse_to_ast(const char* source, SavineAST* ast);

void savine_ast_init(SavineAST* ast);
void savine_ast_free(SavineAST* ast);

#endif
