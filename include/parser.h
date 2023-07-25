#ifndef PARSER_H_
#define PARSER_H_

#include "chunk.h"
#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"
#include "type_set.h"

// typedef enum OrsoTypeNodeType {
//     ORSO_TYPE_NODE_TYPE_UNION,
//     ORSO_TYPE_NODE_TYPE_FUNCTION,
//     ORSO_TYPE_NODE_TYPE_IDENTIFIER,
// } OrsoTypeNodeType;

// typedef enum OrsoExpressionType {
//     EXPRESSION_NONE,
//     EXPRESSION_IMPLICIT_CAST,
//     EXPRESSION_UNARY,
//     EXPRESSION_BINARY,
//     EXPRESSION_GROUPING,
//     EXPRESSION_CALL,
//     EXPRESSION_PRIMARY,
//     EXPRESSION_ENTITY,
//     EXPRESSION_ASSIGNMENT,
//     EXPRESSION_BLOCK,
//     EXPRESSION_IFELSE,
//     EXPRESSION_FUNCTION_DEFINITION,
//     EXPRESSION_FOR,
// } OrsoExpressionType;

// declaration statements are just anonymous declarations
// typedef enum OrsoDeclarationType {
//     ORSO_DECLARATION_NONE,
//     ORSO_DECLARATION_STATEMENT,
//     ORSO_DECLARATION_ENTITY,
// } OrsoDeclarationType;

// typedef enum OrsoStatementType {
//     ORSO_STATEMENT_NONE,
//     ORSO_STATEMENT_PRINT_EXPR,
//     ORSO_STATEMENT_PRINT,
//     ORSO_STATEMENT_EXPRESSION,
//     ORSO_STATEMENT_RETURN,
// } OrsoStatementType;

typedef enum OrsoReturnGuarentee {
    ORSO_NO_RETURN_GUARENTEED,
    ORSO_MAYBE_RETURNS,
    ORSO_RETURN_GUARENTEED,
} OrsoReturnGuarentee;

typedef struct OrsoExpressionNode OrsoExpressionNode;
typedef struct OrsoDeclarationNode OrsoDeclarationNode;
typedef struct OrsoTypeNode OrsoTypeNode;

struct OrsoASTNode;
typedef struct OrsoASTNode OrsoASTNode;

typedef struct OrsoScope {
    OrsoASTNode* creator;
    OrsoSymbolTable named_entities;
    struct OrsoScope* outer;
} OrsoScope;

// struct OrsoTypeNode {
//     Token start;
//     Token end;
//     OrsoTypeNodeType type;
//     union {
//         OrsoTypeNode** union_;
//         struct {
//             OrsoTypeNode* return_type;
//             OrsoTypeNode** argument_types;
//         } function;
//         Token primitive;
//     } items;
// };

// typedef struct OrsoImplicitCastOp {
//     OrsoExpressionNode* operand;
// } OrsoImplicitCastOp;

// typedef struct OrsoBinaryOp {
//     Token operator;
//     OrsoExpressionNode* left;
//     OrsoExpressionNode* right;
// } OrsoBinaryOp;

// typedef struct OrsoUnaryOp {
//     Token operator;
//     OrsoExpressionNode* operand;
// } OrsoUnaryOp;

// typedef struct OrsoGrouping {
//     OrsoExpressionNode* expression;
// } OrsoGrouping;

// typedef struct OrsoCall {
//     OrsoExpressionNode* callee;
//     OrsoExpressionNode** arguments;
// } OrsoCall;

// typedef struct OrsoPrimary {
//     Token token;
//     i32 value_index;
// } OrsoPrimary;

// typedef struct OrsoEntity {
//     Token name;
// } OrsoEntity;

// typedef struct OrsoAssignment {
//     Token name;
//     OrsoExpressionNode* right_side;
// } OrsoAssignment;

// typedef struct OrsoBlock {
//     OrsoDeclarationNode** declarations;
//     OrsoDeclarationNode* final_expression_statement;

//     OrsoReturnGuarentee return_guarentee; 
// } OrsoBlock;

// typedef struct OrsoIfElse {
//     bool is_negated;
//     bool loop_block;
//     OrsoExpressionNode* condition;
//     OrsoExpressionNode* then;
//     OrsoExpressionNode* else_;

//     OrsoReturnGuarentee return_guarentee;
// } OrsoIfElse;

// typedef struct OrsoEntityDeclarationNode {
//     Token start;
//     Token end;

//     bool is_mutable;
//     i32 fold_level_resolved_at;

//     // Unresolved means inferred
//     OrsoType* type;
//     Token name;
//     OrsoTypeNode* type_node;
//     OrsoExpressionNode* expression;
//     i32 implicit_default_value_index;
// } OrsoEntityDeclarationNode;

// typedef struct OrsoFunctionDefinition {
//     OrsoEntityDeclarationNode** parameters;

//     OrsoTypeNode* return_type;
//     OrsoExpressionNode* block_expression;

//     bool cannot_compile;
// } OrsoFunctionDefinition;

// struct OrsoExpressionNode {
//     Token start;
//     Token end;
//     OrsoType* value_type;
//     OrsoType* narrowed_value_type; // only set for union types
//     OrsoExpressionType type;

//     bool fold;

//     // the expression is foldable but is not necessarily folded
//     bool foldable; 

//     // -1 means no value has been folded, >= 0 indicates the index of the value
//     // it's up to the user to know the size of the expression in bits
//     i32 folded_value_index;

//     union {
//         OrsoImplicitCastOp cast;
//         OrsoBinaryOp binary;
//         OrsoUnaryOp unary;
//         OrsoGrouping grouping;
//         OrsoCall call;
//         OrsoPrimary primary;
//         OrsoEntity entity;
//         OrsoAssignment assignment;
//         OrsoBlock block;
//         OrsoIfElse ifelse;
//         OrsoFunctionDefinition function_definition;
//     } expr;
// };

// typedef struct OrsoStatementNode_ {
//     OrsoStatementType type;
//     Token start;
//     Token end;
//     union {
//         OrsoExpressionNode* expression;
//         bool is_print_expr;
//     } stmt;
// } OrsoStatementNode_;

// struct OrsoDeclarationNode {
//     OrsoDeclarationType type;
//     Token start;
//     Token end;
//     union {
//         OrsoStatementNode* statement;
//         OrsoEntityDeclarationNode* entity;
//     } decl;
// };

typedef struct FunctionDefinitionPair {
    OrsoFunction* function;
    OrsoASTNode* ast_defintion;
} FunctionDefinitionPair;

typedef enum OrsoASTNodeType {
    ORSO_AST_NODE_TYPE_UNDEFINED, // TODO: try to remove this shit
    ORSO_AST_NODE_TYPE_DECLARATION,
    ORSO_AST_NODE_TYPE_STATEMENT_RETURN,
    ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION,
    ORSO_AST_NODE_TYPE_STATEMENT_PRINT, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_STATEMENT_PRINT_EXPR, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BINARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_UNARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING,
    ORSO_AST_NODE_TYPE_EXPRESSION_CALL,
    ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK,
    ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING,
    ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION,
} OrsoASTNodeType;

typedef struct OrsoASTFuncionDefinition {
    OrsoASTNode** parameters;
    OrsoASTNode* return_type_expression;

    OrsoASTNode* block;

    bool compilable;
} OrsoASTFunctionDefinition;

typedef struct OrsoASTCall {
    OrsoASTNode* callee;
    OrsoASTNode** arguments;
} OrsoASTCall;

typedef struct OrsoASTBranching {
    OrsoASTNode* condition;
    OrsoASTNode* then_expression;
    OrsoASTNode* else_expression;

    bool looping;
    bool condition_negated;
} OrsoASTBranching;

typedef struct OrsoASTBinary {
    OrsoASTNode* lhs;
    OrsoASTNode* rhs;
} OrsoASTBinary;

typedef struct OrsoASTDeclaration {
    bool is_mutable;
    i32 fold_level_resolved_at;

    OrsoASTNode* identifier;
    OrsoASTNode* type_expression;
    OrsoASTNode* initial_value_expression;
} OrsoASTDeclaration;

struct OrsoASTNode {
    OrsoASTNodeType node_type;

    Token start, end, operator;

    // expressions TODO: Fill this in for *everything*, declarations, statements included
    OrsoType *type, *narrowed_type;

    // branching, blocks
    OrsoReturnGuarentee return_guarentee;

    bool fold;

    bool foldable;

    // primary, folding value
    i32 value_index;


    union {
        OrsoASTDeclaration declaration;

        // statement, print, print_expr, cast, grouping, entity
        OrsoASTNode* expression;

        OrsoASTCall call;

        // binary, assignment
        OrsoASTBinary binary;

        // block (declarations or statements)
        OrsoASTNode** block;

        // branching (if, unless, while, until)
        OrsoASTBranching branch;

        OrsoASTFunctionDefinition function_definition;
    } data;
};

typedef struct OrsoAST {
    bool resolved;
    OrsoTypeSet type_set;

    OrsoSymbolTable builtins;

    FunctionDefinitionPair* function_definition_pairs;

    OrsoASTNode** nodes;

    OrsoASTNode* root;
    OrsoSlot* folded_constants;
    OrsoSymbolTable* symbols;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn);

void orso_ast_init(OrsoAST* ast, OrsoSymbolTable* symbols);
void orso_ast_free(OrsoAST* ast);

OrsoASTNode* orso_ast_node_new(OrsoAST* ast, OrsoASTNodeType node_type, Token start);

bool orso_ast_node_type_is_decl_or_stmt(OrsoASTNodeType node_type);
bool orso_ast_node_type_is_expression(OrsoASTNodeType node_type);

#endif
