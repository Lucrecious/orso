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
    ORSO_TYPE_NODE_TYPE_IDENTIFIER,
} OrsoTypeNodeType;

typedef enum OrsoExpressionType {
    EXPRESSION_NONE,
    EXPRESSION_IMPLICIT_CAST,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_GROUPING,
    EXPRESSION_CALL,
    EXPRESSION_PRIMARY,
    EXPRESSION_ENTITY,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_BLOCK,
    EXPRESSION_IFELSE,
    EXPRESSION_FUNCTION_DEFINITION,
    EXPRESSION_FOR,
} OrsoExpressionType;

// TODO: Reduce declaration type to just declarations
// declaration statements are just anonymous declarations
typedef enum OrsoDeclarationType {
    ORSO_DECLARATION_NONE,
    ORSO_DECLARATION_STATEMENT,
    ORSO_DECLARATION_ENTITY,
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
    i32 value_index;
} OrsoPrimary;

typedef struct OrsoEntity {
    Token name;
} OrsoEntity;

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

typedef struct OrsoEntityDeclarationNode {
    Token start;
    Token end;

    // Unresolved means inferred
    bool is_mutable;
    OrsoType* type;
    Token name;
    OrsoTypeNode* type_node;
    OrsoExpressionNode* expression;
    i32 implicit_default_value_index;

} OrsoEntityDeclarationNode;

typedef struct OrsoFunctionDefinition {
    OrsoEntityDeclarationNode** parameters;

    OrsoTypeNode* return_type;
    OrsoExpressionNode* block_expression;
} OrsoFunctionDefinition;

struct OrsoExpressionNode {
    Token start;
    Token end;
    OrsoType* value_type;
    OrsoType* narrowed_value_type; // only set for union types
    OrsoExpressionType type;

    // the expression is foldable but is not necessarily folded
    bool foldable; 

    // -1 means no value has been folded, >= 0 indicates the index of the value
    // it's up to the user to know the size of the expression in bits
    i32 folded_value_index;

    union {
        OrsoImplicitCastOp cast;
        OrsoBinaryOp binary;
        OrsoUnaryOp unary;
        OrsoGrouping grouping;
        OrsoCall call;
        OrsoPrimary primary;
        OrsoEntity entity;
        OrsoAssignment assignment;
        OrsoBlock block;
        OrsoIfElse ifelse;
        OrsoFunctionDefinition function_definition;
    } expr;
};

typedef struct OrsoStatementNode {
    OrsoStatementType type;
    Token start;
    Token end;
    union {
        OrsoExpressionNode* expression;
        bool is_print_expr;
    } stmt;
} OrsoStatementNode;

struct OrsoDeclarationNode {
    OrsoDeclarationType type;
    Token start;
    Token end;
    union {
        OrsoStatementNode* statement;
        OrsoEntityDeclarationNode* entity;
    } decl;
};

typedef struct OrsoAST {
    bool resolved;
    OrsoTypeSet type_set;
    OrsoDeclarationNode** declarations;
    OrsoSlot* folded_constants;
    OrsoSymbolTable* symbols;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn);

void orso_ast_init(OrsoAST* ast, OrsoSymbolTable* symbols);
void orso_ast_free(OrsoAST* ast);

#endif
