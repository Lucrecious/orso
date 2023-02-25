#ifndef PARSER_H_
#define PARSER_H_

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
    EXPRESSION_VARIABLE,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_BLOCK,
} ExpressionType;

typedef enum OrsoDeclarationType {
    ORSO_DECLARATION_NONE,
    ORSO_DECLARATION_STATEMENT,
    ORSO_DECLARATION_VAR,
} OrsoDeclarationType;

typedef enum OrsoStatementType {
    ORSO_STATEMENT_NONE,
    ORSO_STATEMENT_PRINT_EXPR,
    ORSO_STATEMENT_EXPRESSION,
} OrsoStatementType;

typedef struct OrsoExpressionNode OrsoExpressionNode;
typedef struct OrsoDeclarationNode OrsoDeclarationNode;

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

typedef struct OrsoVariable {
    Token name;
} OrsoVariable;

typedef struct OrsoAssignment {
    Token variable_name;
    OrsoExpressionNode* right_side;
} OrsoAssignment;

typedef struct OrsoBlock {
    OrsoDeclarationNode** declarations;
    OrsoDeclarationNode* final_expression_statement;
} OrsoBlock;

struct OrsoExpressionNode {
    Token start;
    Token end;
    OrsoType value_type;
    OrsoType narrowed_value_type; // only set for union types
    ExpressionType type;
    union {
        OrsoImplicitCastOp cast;
        OrsoBinaryOp binary;
        OrsoUnaryOp unary;
        OrsoGrouping grouping;
        OrsoPrimary primary;
        OrsoVariable variable;
        OrsoAssignment assignment;
        OrsoBlock block;
    } expr;
};

typedef struct OrsoVarDeclarationNode {
    Token start;
    Token end;

    // Unresolved means inferred
    OrsoType var_type;
    Token variable_name;
    Token* type_identifiers;
    OrsoExpressionNode* expression;

} OrsoVarDeclarationNode;

typedef struct OrsoStatementNode {
    OrsoStatementType type;
    Token start;
    Token end;
    union {
        OrsoExpressionNode* expression;
    } stmt;
} OrsoStatementNode;

struct OrsoDeclarationNode {
    OrsoDeclarationType type;
    Token start;
    Token end;
    union {
        OrsoStatementNode* statement;
        OrsoVarDeclarationNode* var;
    } decl;
};

typedef struct OrsoAST {
    OrsoDeclarationNode** declarations;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn);

void orso_ast_init(OrsoAST* ast);
void orso_ast_free(OrsoAST* ast);

#endif
