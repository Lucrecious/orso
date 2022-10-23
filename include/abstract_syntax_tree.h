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

typedef struct OrsoExpressionNode OrsoExpressionNode;

typedef struct OrsoImplicitCastOp {
    OrsoExpressionNode* operand;
} OrsoImplicitCastOp;

typedef struct OrsoBinaryOp {
    Token operator;
    OrsoExpressionNode* left;
    OrsoExpressionNode* right;
} OrsoBinaryOp;

typedef struct OrsoUnaryOp {
    Token operator;
    OrsoExpressionNode* operand;
} OrsoUnaryOp;

typedef struct OrsoGrouping {
    OrsoExpressionNode* expression;
} OrsoGrouping;

typedef struct OrsoPrimary {
    Token token;
    OrsoValue constant;
} OrsoPrimary;

struct OrsoExpressionNode {
    OrsoType value_type;
    ExpressionType type;
    union {
        OrsoImplicitCastOp cast;
        OrsoBinaryOp binary;
        OrsoUnaryOp unary;
        OrsoGrouping grouping;
        OrsoPrimary primary;
    };
};

typedef struct OrsoAST {
    OrsoExpressionNode* expression;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse_to_ast(const char* source, OrsoAST* ast);

void orso_ast_init(OrsoAST* ast);
void orso_ast_free(OrsoAST* ast);

#endif
