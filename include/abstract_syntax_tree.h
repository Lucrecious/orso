#ifndef ABSTRACT_SYNTAX_TREE_H_
#define ABSTRACT_SYNTAX_TREE_H_

#include "lexer.h"

typedef enum ExpressionType {
    EXPRESSION_NONE,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_GROUPING,
    EXPRESSION_PRIMARY,
} ExpressionType;

typedef struct SavineExpressionNode SavineExpressionNode;

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
    i32 value;
} SavinePrimary;

struct SavineExpressionNode {
    ExpressionType type;
    union {
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
