#ifndef PARSER_H_
#define PARSER_H_

#include "chunk.h"
#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"
#include "type_set.h"

typedef enum OrsoTypeNodeType {
    ORSO_TYPE_NODE_TYPE_UNION,
    ORSO_TYPE_NODE_TYPE_FUNCTION,
    ORSO_TYPE_NODE_TYPE_PRIMITIVE,
} OrsoTypeNodeType;

typedef enum OrsoExpressionType {
    EXPRESSION_NONE,
    EXPRESSION_IMPLICIT_CAST,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_GROUPING,
    EXPRESSION_CALL,
    EXPRESSION_PRIMARY,
    EXPRESSION_VARIABLE,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_BLOCK,
    EXPRESSION_IFELSE,
    EXPRESSION_FOR,
} OrsoExpressionType;

typedef enum OrsoDeclarationType {
    ORSO_DECLARATION_NONE,
    ORSO_DECLARATION_STATEMENT,
    ORSO_DECLARATION_VAR,
    ORSO_DECLARATION_FUNCTION,
} OrsoDeclarationType;

typedef enum OrsoStatementType {
    ORSO_STATEMENT_NONE,
    ORSO_STATEMENT_PRINT_EXPR,
    ORSO_STATEMENT_PRINT,
    ORSO_STATEMENT_EXPRESSION,
    ORSO_STATEMENT_RETURN,
} OrsoStatementType;

typedef struct OrsoFunctionSignature {
    OrsoType* return_type;
    OrsoType** parameter_types;
} OrsoFunctionSignature;

typedef struct OrsoExpressionNode OrsoExpressionNode;
typedef struct OrsoDeclarationNode OrsoDeclarationNode;
typedef struct OrsoFunctionDeclarationNode OrsoFunctionDeclarationNode;
typedef struct OrsoTypeNode OrsoTypeNode;

struct OrsoTypeNode {
    Token start;
    Token end;
    OrsoTypeNodeType type;
    union {
        OrsoTypeNode** union_;
        struct {
            OrsoTypeNode* return_type;
            OrsoTypeNode** argument_types;
        } function;
        Token primitive;
    } items;
};

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

typedef struct OrsoCall {
    OrsoType* callee_type;
    OrsoFunctionType* callee_function_type;
    Token callee;
    OrsoExpressionNode** arguments;
} OrsoCall;

typedef struct OrsoPrimary {
    Token token;
    OrsoSlot constant;
} OrsoPrimary;

typedef struct OrsoVariable {
    Token name;
} OrsoVariable;

typedef struct OrsoAssignment {
    Token name;
    OrsoExpressionNode* right_side;
} OrsoAssignment;

typedef struct OrsoBlock {
    OrsoDeclarationNode** declarations;
    OrsoDeclarationNode* final_expression_statement;
} OrsoBlock;

typedef struct OrsoIfElse {
    bool is_negated;
    bool loop_block;
    OrsoExpressionNode* condition;
    OrsoExpressionNode* then;
    OrsoExpressionNode* else_;
} OrsoIfElse;

struct OrsoExpressionNode {
    Token start;
    Token end;
    OrsoType* value_type;
    OrsoType* narrowed_value_type; // only set for union types
    OrsoExpressionType type;
    union {
        OrsoImplicitCastOp cast;
        OrsoBinaryOp binary;
        OrsoUnaryOp unary;
        OrsoGrouping grouping;
        OrsoCall call;
        OrsoPrimary primary;
        OrsoVariable variable;
        OrsoAssignment assignment;
        OrsoBlock block;
        OrsoIfElse ifelse;
    } expr;
};

typedef struct OrsoVariableDeclarationNode {
    Token start;
    Token end;

    // Unresolved means inferred
    OrsoType* type;
    Token name;
    OrsoTypeNode* type_node;
    OrsoExpressionNode* expression;

} OrsoVariableDeclarationNode;

typedef struct OrsoStatementNode {
    OrsoStatementType type;
    Token start;
    Token end;
    union {
        OrsoExpressionNode* expression;
        bool is_print_expr;
    } stmt;
} OrsoStatementNode;

struct OrsoFunctionDeclarationNode {
    Token start;
    Token end;

    OrsoFunctionType* type;

    OrsoVariableDeclarationNode** parameters;

    OrsoTypeNode* return_type;
    Token name;
    OrsoBlock block;
};

struct OrsoDeclarationNode {
    OrsoDeclarationType type;
    Token start;
    Token end;
    union {
        OrsoStatementNode* statement;
        OrsoVariableDeclarationNode* variable;
        OrsoFunctionDeclarationNode* function;
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
