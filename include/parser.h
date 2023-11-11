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

typedef enum OrsoScopeType {
    SCOPE_TYPE_MODULE,
    SCOPE_TYPE_FUNCTION_PARAMETERS,
    SCOPE_TYPE_FUNCTION_BODY,
    SCOPE_TYPE_BLOCK,
    SCOPE_TYPE_STRUCT,
} OrsoScopeType;

typedef struct OrsoScope {
    OrsoASTNode* creator;
    OrsoSymbolTable named_entities;
    OrsoScopeType type;
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
    ORSO_AST_NODE_TYPE_EXPRESSION_PRINT, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BINARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_DOT,
    ORSO_AST_NODE_TYPE_EXPRESSION_UNARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING,
    ORSO_AST_NODE_TYPE_EXPRESSION_CALL,
    ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK,
    ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING,
    ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION,
    ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION,
    ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE,

    /*
    * This is a special case expression. It's simply an expression with a statement inside.
    * This is used for branching.
    */
    ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT,
} OrsoASTNodeType;

#define ORSO_AST_NODE_TYPE_EXPRESSION_CASE \
ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: \
case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: \
case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: \
case ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: \
case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: \
case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: \
case ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_DOT

typedef struct OrsoASTFuncion {
    OrsoASTNode** parameter_nodes;
    OrsoASTNode* return_type_expression;

    OrsoASTNode* block;

    bool compilable;
} OrsoASTFunction;

typedef struct OrsoASTStruct {
    OrsoASTNode** declarations;
} OrsoASTStruct;

typedef struct OrsoASTCall {
    OrsoASTNode* callee;
    OrsoASTNode** arguments;
} OrsoASTCall;

typedef struct OrsoASTBranch {
    OrsoASTNode* condition;
    OrsoASTNode* then_expression;
    OrsoASTNode* else_expression;

    bool looping;
    bool condition_negated;
} OrsoASTBranch;

typedef struct OrsoASTBinary {
    OrsoASTNode* lhs;
    OrsoASTNode* rhs;
} OrsoASTBinary;

typedef struct OrsoASTMemberAccess {
    OrsoASTNode* lhs;

    Token identifier;
    OrsoASTNode* referencing_declaration;
} OrsoASTMemberAccess;

typedef struct OrsoASTDeclaration {
    bool is_mutable;
    i32 fold_level_resolved_at;

    Token identifier;
    OrsoASTNode* type_expression;
    OrsoASTNode* initial_value_expression;
} OrsoASTDeclaration;

struct OrsoASTNode {
    OrsoASTNodeType node_type;

    Token start, end, operator;

    // expressions TODO: Fill this in for *everything*, declarations, statements included
    // TODO: only use value type
    OrsoType *value_type, *value_type_narrowed;
    //AccessIdentifiers *value_type_identifiers, *value_type_narrowed_identifiers;

    // branching, blocks
    OrsoReturnGuarentee return_guarentee;

    bool is_in_type_context;

    bool fold;

    bool foldable;

    // primary, folding value
    i32 value_index;

    union {
        OrsoASTDeclaration declaration;

        // statement, print, print_expr, cast, grouping
        OrsoASTNode* expression;
        OrsoASTNode* statement; // for readability

        OrsoASTCall call;

        // binary, assignment
        OrsoASTBinary binary;

        // block (declarations or statements)
        OrsoASTNode** block;

        // branching (if, unless, while, until)
        OrsoASTBranch branch;

        // function signatures and defintions
        OrsoASTFunction function;

        // structs
        OrsoASTStruct struct_;

        // member access, entity
        OrsoASTMemberAccess dot;
    } data;
};

static khint_t ptr_hash(void* ptr) {
    return kh_int64_hash_func((khint64_t)ptr);
}

static khint32_t ptr_equal(void* a, void* b) {
    return kh_int64_hash_equal((khint64_t)a, (khint64_t)b);
}

KHASH_INIT(ptr2i32, void*, i32, 1, ptr_hash, ptr_equal)

typedef struct OrsoASTNodeAndScope {
    OrsoASTNode* node;
    OrsoScope* scope;
} OrsoASTNodeAndScope;

KHASH_INIT(type2ns, OrsoType*, OrsoASTNodeAndScope, 1, ptr_hash, ptr_equal)

typedef struct OrsoAST {
    i32 void_index;
    i32 true_index;

    bool resolved;
    OrsoTypeSet type_set;

    OrsoSymbolTable builtins;

    FunctionDefinitionPair* function_definition_pairs;

    OrsoASTNode** nodes;

    OrsoASTNode* root;
    OrsoType** folded_constant_types;
    OrsoSlot* folded_constants;

    khash_t(ptr2i32)* type_to_zero_index;
    khash_t(type2ns)* type_to_creation_node;

    OrsoSymbolTable* symbols;
} OrsoAST;

void orso_ast_print(OrsoAST* ast, const char* name);

bool orso_parse(OrsoAST* ast, const char* source, OrsoErrorFunction error_fn);

void orso_ast_init(OrsoAST* ast, OrsoSymbolTable* symbols);
void orso_ast_free(OrsoAST* ast);

OrsoASTNode* orso_ast_node_new(OrsoAST* ast, OrsoASTNodeType node_type, bool is_in_type_context, Token start);

bool orso_ast_node_type_is_decl_or_stmt(OrsoASTNodeType node_type);
bool orso_ast_node_type_is_expression(OrsoASTNodeType node_type);

#endif
