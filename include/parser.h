#ifndef ABSTRACT_SYNTAX_TREE_H_
#define ABSTRACT_SYNTAX_TREE_H_

#include "chunk.h"
#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"

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
    OrsoSlot constant;
} OrsoPrimary;

struct OrsoExpressionNode {
    Token start;
    Token end;
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

typedef struct OrsoStatement {
    union {
        struct {
            OrsoExpressionNode* expression;
        } print_expr;
    };
} OrsoStatement;

typedef struct OrsoDeclarationNode {
    Token start;
    Token end;
    union {
        OrsoStatement statement;
    };
} OrsoDeclarationNode;

typedef struct OrsoAST {
    OrsoDeclarationNode** declarations;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse(OrsoAST* ast, const char* source, OrsoSymbolTable* symbol_table, OrsoErrorFunction error_fn);

void orso_ast_init(OrsoAST* ast);
void orso_ast_free(OrsoAST* ast);

#endif
