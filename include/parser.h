#ifndef PARSER_H_
#define PARSER_H_

#include "chunk.h"
#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"
#include "type_set.h"

typedef enum OrsoReturnGuarentee {
    ORSO_NO_RETURN_GUARENTEED,
    ORSO_MAYBE_RETURNS,
    ORSO_RETURN_GUARENTEED,
} OrsoReturnGuarentee;

struct OrsoASTNode;
typedef struct OrsoASTNode OrsoASTNode;

typedef struct OrsoScope {
    OrsoASTNode* creator;
    OrsoSymbolTable named_entities;
    struct OrsoScope* outer;
} OrsoScope;

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
