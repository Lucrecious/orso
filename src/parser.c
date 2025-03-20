#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "error.h"
#include "type.h"
#include "type_set.h"
#include "stringt.h"
#include "tmp.h"
#include "../nob.h"

/*
program                  -> declaration* EOF
declaration              -> definition | statement
definition               -> IDENTIFIER `:` ((logic_or ((`=` | `:`) expression)?) | ((`=` | `:`) expression) `;`
parameters               -> parameter (`,` parameter)*
parameter                -> IDENTIFIER `:` ((logic_or (`=` expression)?)
statement                -> expression_statement
expression_statement     -> expression `;`

directive                -> `#` ~(\s)*
expression               -> directive? (assignment | block | branch)
block                    -> `{` declaration* `}`
branch                   -> (`if` | `unless` | `while` | `until` ) expression (block | (`then` | `do`) expression) (`else` expression)?

assignment               -> (call `.`)? IDENTIFIER `=` expression
                          | logic_or
logic_or                 -> logic_and (`or` logic_and)*
logic_and                -> equality (`and` equality)*
equality                 -> comparison ((`!=` | `==`) comparison)*
comparison               -> term ((`<` | `>` | `<=` | `>=`) term)*
term                     -> factor ((`+` | `-`) factor)*
factor                   -> logical_operations ((`/` | `*`) logical_operations)*
logical_operations       -> unary ((`|` |`&`) unary)*
unary                    -> (`not` | `-` | `&`) unary | call
call                     -> primary ( `(` arguments? `)` ) | `.` IDENTIFIER )*
arguments                -> argument (`,` argument)*
argument                 -> (IDENTIFIER `:`)? expression

primary                  -> `true` | `false` | `null` | IDENTIFIER | INTEGER | DECIMAL
                          | STRING | SYMBOL | `(` expression `)` | function_definition | function_type | struct_definition 
                          | type_init
function_definition      -> `(` parameters? `)` `->` logic_or block
function_type            -> `(` (logic_or (`,` logic_or)*)? `)` `->` logic_or
struct_definition        -> `struct` `{` definition* `}`
type_init                ->  logic_or`{` `}`
*/

#define X(error_type, error_message, error_source) [error_type] = error_message,
cstr_t const error_messages[] = {
#include "error.x"
};
#undef X

#define X(error_type, error_message, error_source) [error_type] = ERROR_SOURCE_##error_source,
error_source_t error_sources[] = {
#include "error.x"
};
#undef X



static int type_hash(type_t id) {
    return kh_int64_hash_func((khint64_t)id.i);
}

static int type_equal_(type_t a, type_t b) {
    return typeid_eq(a, b);
}

implement_table(t2w, type_t, word_t, type_hash, type_equal_)
implement_table(type2ns, type_t, ast_node_and_scope_t, type_hash, type_equal_)

static bool streq___(const string_t a, const string_t b) {
    return string_eq(a, b);
}

uint32_t fnv1_hash__(string_t s) {
	uint32_t hash = 0x811C9DC5;

	for (size_t i = 0; i < s.length; i++) {
		hash *= 0x01000193;
		hash ^= (uint32_t)(unsigned char)s.cstr[i];
	}

	return hash;
}

implement_table(s2w, string_t, word_t, fnv1_hash__, streq___)
implement_table(s2n, string_t, ast_node_t*, fnv1_hash__, streq___)

static khint64_t hashptr_(void *ptr) {
    return (u64)ptr;
}

static bool hasheq_(void *a, void *b) {
    return a == b;
}

implement_table(p2s, void*, string_t, hashptr_, hasheq_);

typedef struct parser_t {
    ast_t *ast;
    lexer_t lexer;
    token_t previous;
    token_t current;

    bool inside_type_context;

    bool had_error;
    bool panic_mode;
} parser_t;

typedef enum {
    PREC_NONE,
    PREC_BLOCK,       // {}, branch expressions (if/unless/while/until else)
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_CAST,        // as
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_BITWISE_OR,  // | and type separator
    PREC_UNARY,       // - not &
    PREC_CALL,        // . ()
    PREC_PRIMARY
} prec_t;

typedef ast_node_t* (*ParseFn)(parser_t*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    prec_t precedence;
} parse_rule_t;

void ast_init(ast_t *ast, arena_t *arena) {
    *ast = zer0(ast_t);
    ast->arena = arena;
    ast->resolved = false;
    
    type_set_init(&ast->type_set, ast->arena);

    memarr_init(&ast->multiword_data, megabytes(0.5));

    ast->errors = (errors_t){.allocator=arena};

    ast->builtins = table_new(s2w, ast->arena);
    ast->intrinsic_fns = table_new(s2w, ast->arena);
    ast->intrinsicfn2cname = table_new(p2s, ast->arena);
    ast->moduleid2node = table_new(s2n, ast->arena);

    ast->type_to_zero_word = table_new(t2w, ast->arena);
    ast->type_to_creation_node = table_new(type2ns, ast->arena);
}

ast_node_t nil_node = {
    .node_type=AST_NODE_TYPE_NONE,
    .start = nil_token,
    .end = nil_token,
    .operator = nil_token,

    .value_type = typeid(TYPE_INVALID),
    .lvalue_node = &nil_node,
    .ref_decl = &nil_node,
    .jmp_out_scope_node = &nil_node,
    .branch_type = BRANCH_TYPE_IFTHEN,
    .ccode_break_label = lit2str(""),
    .ccode_continue_label = lit2str(""),
    .ccode_var_name = lit2str(""),
    .identifier = nil_token,
    .last_statement = &nil_node,
};

ast_node_t *ast_node_new(ast_t *ast, ast_node_type_t node_type, token_t start) {
    ast_node_t *node = (ast_node_t*)arena_alloc(ast->arena, sizeof(ast_node_t));
    *node = (ast_node_t){0};
    
    node->node_type = node_type;
    node->start = start;
    node->end = start;
    node->operator = start;
    node->value_type.i = TYPE_UNRESOLVED;
    node->condition_negated = false;
    node->branch_type = BRANCH_TYPE_IFTHEN;
    node->requires_tmp_for_cgen = true;
    node->vm_jmp_index = 0;
    node->ccode_break_label = lit2str("");
    node->ccode_continue_label = lit2str("");
    node->ccode_var_name = lit2str("");

    node->foldable = false;
    node->fold_level_resolved_at = -1;
    node->is_mutable = false;
    node->is_exported = false;
    node->is_intrinsic = false;
    node->identifier = nil_token;
    node->ref_decl = &nil_node;

    node->jmp_nodes.allocator = ast->arena;
    node->jmp_out_scope_node = &nil_node;

    node->lvalue_node = &nil_node;
    node->expr_val = ast_node_val_nil();
    node->is_free_number = false;

    node->last_statement = &nil_node;

    node->children = (ast_nodes_t){.allocator=ast->arena};
    node->realized_funcdef_copies = (inferred_funcdef_copies_t){.allocator=ast->arena};
    node->type_decl_patterns = (type_patterns_t){.allocator=ast->arena};

    switch (node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS:
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }
        
        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            array_push(&node->children, &nil_node);
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_UNARY:
        case AST_NODE_TYPE_EXPRESSION_CAST:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_EXPRESSION_JMP:
        case AST_NODE_TYPE_EXPRESSION_GROUPING:
        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE:
        case AST_NODE_TYPE_EXPRESSION_CALL: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            UNREACHABLE();
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
            break;
        
        case AST_NODE_TYPE_EXPRESSION_NIL: {
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE:
        case AST_NODE_TYPE_EXPRESSION_DOT: {
            array_push(&node->children, &nil_node);
            node->identifier = nil_token;
            break;
        }

        case AST_NODE_TYPE_NONE: break;// UNREACHABLE();
    }

    return node;
}

type_path_t *type_path_copy(type_path_t *path, arena_t *arena) {
    if (path == NULL) return NULL;

    type_path_t *new_path = arena_alloc(arena, sizeof(type_path_t));
    *new_path = *path;
    new_path->next = type_path_copy(new_path->next, arena);

    return new_path;
}

ast_node_t *ast_node_copy(ast_t *ast, ast_node_t *node) {
    ast_node_t *copy = ast_node_new(ast, node->node_type, node->start);

    copy->branch_type = node->branch_type;
    copy->ccode_break_label = node->ccode_break_label;
    copy->ccode_continue_label = node->ccode_continue_label;
    copy->ccode_init_func_name = node->ccode_init_func_name;
    copy->ccode_var_name = node->ccode_var_name;

    for (size_t i = copy->children.count; i < node->children.count; ++i) {
        array_push(&copy->children, &nil_node);
    }

    for (size_t i = 0; i < node->children.count; ++i) {
        ast_node_t *child_copy = node->children.items[i];
        child_copy = ast_node_copy(ast, child_copy);
        copy->children.items[i] = child_copy;
    }

    copy->condition_negated = node->condition_negated;
    copy->end = node->end;
    copy->expr_val = node->expr_val;
    copy->fold_level_resolved_at = node->fold_level_resolved_at;
    copy->foldable = node->foldable;
    copy->identifier = node->identifier;

    copy->is_exported = node->is_exported;
    copy->is_free_number = node->is_free_number;
    copy->is_intrinsic = node->is_intrinsic;
    copy->is_mutable = node->is_mutable;

    ASSERT(copy->jmp_nodes.count == 0, "need to convert references to other nodes to relative ones");
    ASSERT(copy->jmp_out_scope_node == &nil_node, "need to convert references to other nodes to relative ones");
    ASSERT(copy->ref_decl == &nil_node, "need to convert references to other nodes to relative ones");
    ASSERT(copy->lvalue_node== &nil_node, "need to convert references to other nodes to relative ones");
    ASSERT(copy->last_statement= &nil_node, "need to convert references to other nodes to relative ones");

    copy->operator = node->operator;

    copy->realized_funcdef_copies = node->realized_funcdef_copies;
    copy->requires_tmp_for_cgen = node->requires_tmp_for_cgen;
    copy->start = node->start;

    for (size_t i = 0; i < node->type_decl_patterns.count; ++i) {
        type_pattern_t pattern = node->type_decl_patterns.items[i];

        pattern.expected = type_path_copy(pattern.expected, ast->arena);

        array_push(&copy->type_decl_patterns, pattern);
    }

    copy->value_type = node->value_type;
    copy->vm_jmp_index = node->vm_jmp_index;
    copy->vm_stack_point = node->vm_stack_point;

    return copy;
}

ast_node_t *ast_begin_module(ast_t *ast) {
    ast_node_t *module = ast_node_new(ast, AST_NODE_TYPE_MODULE, nil_token);
    return module;
}

void ast_end_module(ast_node_t *module) {
    if (module->children.count == 0) return;

    token_t min_token = module->children.items[0]->start;
    token_t max_token = module->children.items[0]->end;
    for (size_t i = 1; i < module->children.count; ++i) {
        token_t start = module->children.items[i]->start;
        token_t end = module->children.items[i]->end;

        if (start.loc.line < min_token.loc.line
        || (start.loc.line == min_token.loc.line && start.loc.column < min_token.loc.column)) min_token = start;

        if (end.loc.line > max_token.loc.line
        || (end.loc.line == max_token.loc.line && end.loc.column > max_token.loc.column)) max_token = end;
    }

    module->start = min_token;
    module->end = max_token;
}

void ast_add_module(ast_t *ast, ast_node_t *module, string_t moduleid) {
    moduleid = string_copy(moduleid, ast->arena);
    table_put(s2n, ast->moduleid2node, moduleid, module);
}

ast_node_t *ast_implicit_expr(ast_t *ast, type_t type, word_t value, token_t where) {
    ast_node_t *expr = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, where);
    expr->value_type = type;
    expr->expr_val = ast_node_val_word(value);
    return expr;
}

ast_node_t *ast_decldef(ast_t *ast, token_t identifier, ast_node_t *type_expr, ast_node_t *init_expr) {
    ast_node_t *definition_node = ast_node_new(ast, AST_NODE_TYPE_DECLARATION_DEFINITION, identifier);
    definition_node->identifier = identifier;
    an_decl_type(definition_node) = type_expr;
    an_decl_expr(definition_node) = init_expr;

    definition_node->start = identifier;
    definition_node->end = init_expr->end;

    return definition_node;
}

ast_node_t *ast_nil(ast_t *ast, type_t value_type, token_t token_location) {
    ast_node_t *nil_node = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_NIL, token_location);
    nil_node->value_type = value_type;
    nil_node->expr_val = zero_value(ast, value_type);
    return nil_node;
}

ast_node_t *ast_def_value(ast_t *ast, token_t identifier) {
    ast_node_t *def_value = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_DEF_VALUE, identifier);
    def_value->identifier = identifier;
    return def_value;
}

ast_node_t *ast_cast(ast_t *ast, ast_node_t *type_expr, ast_node_t *expr) {
    ast_node_t *cast_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST, type_expr->start);
    cast_->end = expr->end;

    an_cast_type(cast_) = type_expr;
    an_cast_expr(cast_) = expr;
    return cast_;
}

static ast_node_t *ast_primaryu(ast_t *ast, u64 value, type_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        if (value < UINT_MAX) {
            type = ast->type_set.uint_;
        } else {
            type = ast->type_set.u64_;
        }
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    word_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: {
        u8 v = cast(u8, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_16: {
        u16 v = cast(u16, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_32: {
        u32 v = cast(u32, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.u = value;
        break;
    }

    default: UNREACHABLE();
    }

    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->value_type = type;
    primary->expr_val = ast_node_val_word(word);

    return primary;
}

static ast_node_t *ast_primaryi(ast_t *ast, s64 value, type_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        if (value >= INT_MIN && value <= INT_MAX) {
            type = ast->type_set.int_;
        } else {
            type = ast->type_set.s64_;
        }
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    word_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: {
        s8 v = cast(s8, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_16: {
        s16 v = cast(s16, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_32: {
        s32 v = cast(s32, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.s = value;
        break;
    }

    default: UNREACHABLE();
    }

    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->expr_val = ast_node_val_word(word);
    primary->value_type = type;

    return primary;
}

static ast_node_t *ast_primaryf(ast_t *ast, f64 value, type_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        type = ast->type_set.f32_;
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    word_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: UNREACHABLE(); break;
    case NUM_SIZE_16: UNREACHABLE(); break;
    case NUM_SIZE_32: {
        f32 v = (f32)value;
        word.as.d = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.d = value;
        break;
    }
    }

    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->expr_val = ast_node_val_word(word);
    primary->value_type = type;

    return primary;
}

static ast_node_t *ast_primaryb(ast_t *ast, bool value, ast_node_t extra_params) {
    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, extra_params.start);

    u8 byte_value = (u8)value;
    word_t word = {.as.u=byte_value};

    primary->expr_val = ast_node_val_word(word);
    primary->value_type = typeid(TYPE_BOOL);

    return primary;
}

static ast_node_t *ast_statement(ast_t *ast, ast_node_t *expr) {
    ast_node_t *statement_node = ast_node_new(ast, AST_NODE_TYPE_DECLARATION_STATEMENT, expr->start);
    an_expression(statement_node) = expr;
    statement_node->end = expr->end;
    return statement_node;
}

static ast_node_t *ast_break(ast_t *ast, ast_node_t *expr, token_t label, token_t start) {
    ast_node_t *break_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_JMP, start);
    an_expression(break_) = expr;
    break_->identifier = label;
    break_->end = expr->end;
    return break_;
}

static ast_node_t *ast_continue(ast_t *ast, token_t label, token_t start) {
    ast_node_t *continue_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_JMP, start);
    an_expression(continue_) = ast_nil(ast, typeid(TYPE_VOID), token_implicit_at_end(label));
    continue_->identifier = label;
    continue_->end = label;
    return continue_;
}

static ast_node_t *ast_return(ast_t *ast, ast_node_t *expr, token_t start) {
    ast_node_t *return_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_JMP, start);
    an_expression(return_) = expr;
    return_->end = expr->end;
    return return_;
}

static ast_node_t *ast_block_begin(ast_t *ast, token_t start) {
    ast_node_t *block = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BLOCK, start);
    return block;
}

static void ast_block_decl(ast_node_t *block, ast_node_t *decl) {
    array_push(&block->children, decl);
}

static void ast_block_end(ast_node_t *block, token_t end) {
    block->end = end;

    if (block->children.count == 0) {
        block->node_type = AST_NODE_TYPE_EXPRESSION_NIL;
        block->value_type = typeid(TYPE_VOID);
    }
}

static ast_node_t *ast_while(ast_t *ast, ast_node_t *cond, bool cond_negated, ast_node_t *then, ast_node_t *else_, token_t start) {
    ast_node_t *while_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BRANCHING, start);
    while_->branch_type = BRANCH_TYPE_LOOPING;
    while_->condition_negated = cond_negated;
    an_condition(while_) = cond;
    an_then(while_) = then;
    an_else(while_) = else_;

    while_->end = else_->end;

    return while_;
}

static ast_node_t *ast_ifthen(ast_t *ast, ast_node_t *cond, bool cond_negated, ast_node_t *then, ast_node_t *else_, token_t start) {
    ast_node_t *ifthen = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BRANCHING, start);
    ifthen->branch_type = BRANCH_TYPE_IFTHEN;
    ifthen->condition_negated = cond_negated;
    an_condition(ifthen) = cond;
    an_then(ifthen) = then;
    an_else(ifthen) = else_;

    ifthen->end = else_->end;

    return ifthen;
}

static ast_node_t *ast_do(ast_t *ast, token_t label, ast_node_t *then, ast_node_t *else_, token_t start) {
    ast_node_t *do_ = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BRANCHING, start);
    do_->branch_type = BRANCH_TYPE_DO;
    do_->identifier = label;
    an_condition(do_) = ast_nil(ast, typeid(TYPE_BOOL), token_implicit_at_end(start));
    an_then(do_) = then;
    an_else(do_) = else_;
    do_->end = else_->end;
    return do_;
}

ast_node_t *ast_inferred_type_decl(ast_t *ast, token_t squiggle_token, token_t identifer) {
    ast_node_t *inferred_type_decl = ast_node_new(ast, AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL, squiggle_token);
    inferred_type_decl->end = identifer;
    inferred_type_decl->identifier = identifer;
    inferred_type_decl->is_mutable = false;
    inferred_type_decl->value_type = typeid(TYPE_UNRESOLVED);

    return inferred_type_decl;
}

static ast_node_t *ast_binary(ast_t *ast, token_t operator, ast_node_t *left, ast_node_t *right) {
    ast_node_t *binary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BINARY, left->start);
    binary->operator = operator;
    binary->end = right->end;
    an_lhs(binary) = left;
    an_rhs(binary) = right;

    return binary;
}

static ast_node_t *ast_array_type(ast_t *ast, ast_node_t *size_expr, ast_node_t *type_expr) {
    ast_node_t *array_size = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE, size_expr->start);
    an_array_size_expr(array_size) = size_expr;
    an_array_type_expr(array_size) = type_expr;
    array_size->end = type_expr->end;
    return array_size;
}

static ast_node_t *ast_item_access(ast_t *ast, ast_node_t *accessee, ast_node_t *accessor) {
    ast_node_t *item_access = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS, accessee->start);
    an_item_accessee(item_access) = accessee;
    an_item_accessor(item_access) = accessor;
    item_access->end = accessor->end;
    return item_access;
}

static ast_node_t *ast_unary(ast_t *ast, token_t operator, ast_node_t *operand) {
    ast_node_t *unary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_UNARY, operator);
    unary->operator = operator;
    unary->end = operand->end;
    an_operand(unary) = operand;
    return unary;
}

static ast_node_t *ast_for(ast_t *ast, ast_node_t *decl, ast_node_t *cond, ast_node_t *incr, ast_node_t *loop, ast_node_t *then, token_t start) {
    ast_node_t *n = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_BRANCHING, start);
    n->branch_type = BRANCH_TYPE_FOR;
    an_for_decl(n) = decl;
    an_condition(n) = cond;
    an_for_incr(n) = incr;
    an_then(n) = loop;
    an_else(n) = then;

    return n;
}

static void parser_init(parser_t *parser, ast_t *ast, string_t file_path, string_view_t source) {
    lexer_init(&parser->lexer, file_path, source);
    parser->ast = ast;
    parser->had_error = false;
    parser->panic_mode = false;
    parser->inside_type_context = false;
}

static void parser_error(parser_t *parser, error_t error) {
    if (parser->panic_mode) return;

    unless (error.is_warning) {
        parser->had_error = true;
        parser->panic_mode = true;
        array_push(&parser->ast->errors, error);
    }

}

static void advance(parser_t *parser) {
    parser->previous = parser->current;
    parser->current = lexer_next_token(&parser->lexer);
}

static bool consume(parser_t *parser, token_type_t type) {
    if (parser->current.type == type) {
        advance(parser);
        return true;
    }
    return false;
}

static FORCE_INLINE bool check(parser_t* parser, token_type_t type) {
    return parser->current.type == type;
}

static bool match(parser_t* parser, token_type_t type) {
    if (!check(parser, type)) {
        return false;
    }

    advance(parser);
    return true;
}

static void synchronize(parser_t* parser) {
    parser->panic_mode = false;

    until (check(parser, TOKEN_EOF)) {
        switch (parser->previous.type) {
            case TOKEN_SEMICOLON:
                return;
            
            default: break;
        }

        switch (parser->current.type) {
            case TOKEN_BRACE_CLOSE:
                return;
            
            default: break;
        }

        advance(parser);

    }
}

static ast_node_t *parse_decl(parser_t* parser, bool is_top_level);
static ast_node_t *parse_expression(parser_t* parser);
static ast_node_t *parse_statement(parser_t* parser);
static parse_rule_t *parser_get_rule(token_type_t type);
static bool check_expression(parser_t* parser);
static ast_node_t* parse_precedence(parser_t* parser, prec_t precedence);

bool ast_node_type_is_expression(ast_node_type_t node_type) {
    switch (node_type) {
        case AST_NODE_TYPE_EXPRESSION_CASE:
            return true;
        
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_NONE:
            return false;
    }
}

token_t token_implicit_at_start(token_t token) {
    token.view.length = 0;
    return token;
}

token_t token_implicit_at_end(token_t token) {
    token.view.data += token.view.length;
    token.loc.column += token.view.length;
    token.view.length = 0;
    return token;
}

static ast_node_t *parse_number(parser_t *parser) {
    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            token_t token = parser->previous;
            string_view_t numsv = token.view;
            // todo: check for overflow on u64 and s64
            u64 value = cstrn_to_u64(numsv.data, numsv.length);

            bool is_free_number = false;

            type_t type = typeid(TYPE_UNRESOLVED);
            if (sv_ends_with(numsv, "s")) {
                type = parser->ast->type_set.int_;
            } else if (sv_ends_with(numsv, "u")) {
                type = parser->ast->type_set.uint_;
            } else if (sv_ends_with(numsv, "s8")) {
                type = parser->ast->type_set.s8_;
            } else if (sv_ends_with(numsv, "u8")) {
                type = parser->ast->type_set.u8_;
            } else if (sv_ends_with(numsv, "s16")) {
                type = parser->ast->type_set.s16_;
            } else if (sv_ends_with(numsv, "u16")) {
                type = parser->ast->type_set.u16_;
            } else if (sv_ends_with(numsv, "s32")) {
                type = parser->ast->type_set.s32_;
            } else if (sv_ends_with(numsv, "u32")) {
                type = parser->ast->type_set.u32_;
            } else if (sv_ends_with(numsv, "s64")) {
                type = parser->ast->type_set.s64_;
            } else if (sv_ends_with(numsv, "u64")) {
                type = parser->ast->type_set.u64_;
            } else if (sv_ends_with(numsv, "sz")) {
                type = parser->ast->type_set.size_t_;
            } else if (sv_ends_with(numsv, "pd")) {
                type = parser->ast->type_set.ptrdiff_t_;
            } else {
                is_free_number = true;
            }

            bool do_signed = true;
            unless (TYPE_IS_UNRESOLVED(type)) {
                typedata_t *td = type2typedata(&parser->ast->type_set.types, type);
                do_signed = (td->as.num == NUM_TYPE_SIGNED);

                switch((num_size_t)td->size) {
                case NUM_SIZE_8: {
                    if (do_signed) {
                        if (value > INT8_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in an 's8'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    } else {
                        if (value > UINT8_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u8'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    }
                    break;
                }

                case NUM_SIZE_16: {
                    if (do_signed) {
                        if (value > INT16_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 's16'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    } else {
                        if (value > UINT16_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u16'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    }
                    break;
                }

                case NUM_SIZE_32: {
                    if (do_signed) {
                        if (value > INT32_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 's32'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    } else {
                        if (value > UINT32_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = WARNING_PARSER_NUMBER_OVERFLOW,
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u32'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    }
                    break;
                }

                case NUM_SIZE_64: break;
                }
            }


            ast_node_t *primary;
            if (do_signed) {
                primary = ast_primaryi(parser->ast, value, type, parser->previous);
            } else {
                primary = ast_primaryu(parser->ast, value, type, parser->previous);
            }
            primary->is_free_number = is_free_number;

            return primary;
        }

        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.view.data, parser->previous.view.length);

            bool is_number_free = false;
            type_t type = parser->ast->type_set.f64_;
            if (sv_ends_with(parser->previous.view, "f")) {
                type = parser->ast->type_set.f32_;
            } else if (sv_ends_with(parser->previous.view, "d")) {
                type = parser->ast->type_set.f64_;
            } else {
                is_number_free = true;
            }

            ast_node_t *primary = ast_primaryf(parser->ast, value, type, parser->previous);
            primary->is_free_number = is_number_free;
            return primary;
        }

        default: UNREACHABLE();
    }
}

static ast_node_t *parse_jmp(parser_t *parser) {
    token_t start_token = parser->previous;
    token_type_t jmp_type = parser->previous.type;

    token_t label = token_implicit_at_end(start_token);
    if (jmp_type == TOKEN_BREAK || jmp_type == TOKEN_CONTINUE) {
        if (match(parser, TOKEN_COLON)) {
            unless (consume(parser, TOKEN_IDENTIFIER)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_EXPECTED_JMP_LABEL_AFTER_COLON,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected a jmp label after ':'"),
                    .args = ORERR_ARGS(error_arg_token(parser->previous)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
            label = parser->previous;
        }
    }

    ast_node_t *expr = NULL;

    bool has_expr = jmp_type == TOKEN_BREAK || jmp_type == TOKEN_RETURN;
    if (has_expr && check_expression(parser)) {
        expr = parse_expression(parser);
    } else {
        expr = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(parser->previous));
    }


    switch (jmp_type) {
        case TOKEN_BREAK: return ast_break(parser->ast, expr, label, start_token);
        case TOKEN_CONTINUE: return ast_continue(parser->ast, label, start_token);
        case TOKEN_RETURN: return ast_return(parser->ast, expr, start_token);
        default: UNREACHABLE();
    }
}

static ast_node_t *parse_literal(parser_t *parser) {
    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            bool is_true = parser->previous.type == TOKEN_TRUE;
            return ast_primaryb(parser->ast, is_true, (ast_node_t) {
                .start = parser->previous,
            });
            break;
        }

        case TOKEN_STRING: {
            UNREACHABLE();
            break;
        };

        case TOKEN_SYMBOL: {
            // todo
            UNREACHABLE();
            break;
        }

        default: UNREACHABLE();
    }
}

static void parse_arguments(parser_t* parser, ast_node_t *parent) {
    unless (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            ast_node_t *argument = parse_expression(parser);
            array_push(&parent->children, argument);
        } while (match(parser, TOKEN_COMMA));
    }

    unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected close parenthesis"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }
}

static ast_node_t *parse_builtin_call(parser_t *parser) {
    token_t identifier = parser->previous;

    unless (consume(parser, TOKEN_PARENTHESIS_OPEN)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_OPEN_PARENTHESIS_BUILTIN_FUNC,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected open parenthesis after builtin function name"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *n = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL, identifier);
    n->children.count = 0;
    n->identifier = identifier;
    parse_arguments(parser, n);
    n->end = parser->previous;

    return n;
}

static ast_node_t *ast_assignment(ast_t *ast, ast_node_t *lhs, ast_node_t *rhs, token_t equals) {
    ast_node_t *assignment = ast_binary(ast, equals, lhs, rhs);
    assignment->node_type = AST_NODE_TYPE_EXPRESSION_ASSIGNMENT;
    return assignment;
}

static ast_node_t *convert_assignment_expression(parser_t *parser, ast_node_t *left, ast_node_t *assignment) {
    (void)parser;
    assignment->start = left->start;
    an_lhs(assignment) = left;

    return assignment;
}

static ast_node_t *convert_call_expression(ast_node_t *left_operand, ast_node_t *call) {
    call->start = left_operand->start;
    an_callee(call) = left_operand;
    return call;
}

static ast_node_t *convert_function_definition(parser_t *parser, ast_node_t *left_operand, ast_node_t *func_def) {
    for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
        ast_node_t *parameter = left_operand->children.items[i];
        if (parameter->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSEREX_EXPECTED_DECLARATION,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("only declarations are allowed as function parameters"),
                .args = ORERR_ARGS(error_arg_node(parameter)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    ast_node_t *block = an_func_def_block(func_def);

    func_def->children.count = 0;
    func_def->start = left_operand->start;

    array_push(&func_def->children, an_func_def_return(left_operand));
    array_push(&func_def->children, block);

    for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
        array_push(&func_def->children, left_operand->children.items[i]);
    }

    // prevents freeing the node  parameter and return expressions
    left_operand->node_type = AST_NODE_TYPE_NONE;

    return func_def;
}

static ast_node_t *parse_assignment(parser_t *parser) {
    token_t equals = parser->previous;

    ast_node_t *rhs = parse_expression(parser);

    ast_node_t *assignment = ast_assignment(parser->ast, &nil_node, rhs, equals);

    return assignment;
}

static ast_node_t *parse_def_value(parser_t *parser) {
    ast_node_t *def_value = ast_def_value(parser->ast, parser->previous);
    return def_value;
}

static ast_node_t *parse_block(parser_t *parser) {
    ast_node_t *block = ast_block_begin(parser->ast, parser->previous);

    until (check(parser, TOKEN_BRACE_CLOSE) || check(parser, TOKEN_EOF)) {
        if (match(parser, TOKEN_SEMICOLON)) {
            continue;
        }

        ast_node_t *decl_node = parse_decl(parser, false);

        unless (consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_SEMICOLON,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after declaration"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        ast_block_decl(block, decl_node);

        synchronize(parser);
    }

    unless (consume(parser, TOKEN_BRACE_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_CLOSE_BRACE,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected '}' to close block"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_block_end(block, parser->previous);

    return block;
}

static ast_node_t *parse_branch(parser_t *parser) {
    token_t start_token = parser->previous;
    token_type_t branch_token_type = parser->previous.type;
    bool condition_negated = false;
    ast_branch_type_t branch_type = BRANCH_TYPE_IFTHEN;

    ast_node_t *condition = NULL;
    ast_node_t *then_branch = NULL;
    ast_node_t *else_branch = NULL;

    ast_node_t *decl_block = NULL;

    token_t label = nil_token;


    if (branch_token_type == TOKEN_DO) {
        branch_type = BRANCH_TYPE_DO;

        if (match(parser, TOKEN_COLON)) {
            unless (consume(parser, TOKEN_IDENTIFIER)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_EXPECTED_JMP_LABEL_AFTER_COLON,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected identifier after ':' on a do expression"),
                    .args = ORERR_ARGS(error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
            label = parser->previous;
        }

        then_branch = parse_expression(parser);

        unless (match(parser, TOKEN_THEN)) {
            else_branch = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(then_branch->end));
        } else {
            else_branch = parse_expression(parser);
        }
    } else {
        if (branch_token_type == TOKEN_UNLESS || branch_token_type == TOKEN_UNTIL) {
            condition_negated = true;
        }

        if (branch_token_type == TOKEN_WHILE || branch_token_type == TOKEN_UNTIL) {
            branch_type = BRANCH_TYPE_LOOPING;
        } else {
            branch_type = BRANCH_TYPE_IFTHEN;
        }

        decl_block = ast_block_begin(parser->ast, start_token);

        // declares before condition for blocks 
        ast_node_t *last_decl;
        while (true) {
            last_decl = parse_decl(parser, false);

            if (check(parser, TOKEN_BRACE_OPEN) || (check(parser, TOKEN_DO) || check(parser, TOKEN_THEN))) break;

            unless (consume(parser, TOKEN_SEMICOLON)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_EXPECTED_SEMICOLON,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected ';' after initial '$1.kind$' expressions"),
                    .args = ORERR_ARGS(error_arg_token(parser->current), error_arg_token(start_token)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }

            ast_block_decl(decl_block, last_decl);
        }

        condition = last_decl;
        if (last_decl->node_type != AST_NODE_TYPE_DECLARATION_STATEMENT) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_EXPRESSION,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected expression as the condition for '$0.kind$' branch"),
                .args = ORERR_ARGS(error_arg_token(start_token), error_arg_node(last_decl)),
                .show_code_lines = ORERR_LINES(1),
            ));
        } else {
            condition = an_expression(last_decl);
        }

        // condition = parse_expression(parser);
        if (match(parser, TOKEN_BRACE_OPEN)) {
            then_branch = parse_block(parser);
        } else {
            if (branch_type == BRANCH_TYPE_LOOPING) {
                unless (consume(parser, TOKEN_DO)) {
                    parser_error(parser, OR_ERROR(
                        .tag = ERROR_PARSER_EXPECTED_THEN_OR_BRACE_AFTER_BRANCH_CONDITION,
                        .level = ERROR_SOURCE_PARSER,
                        .msg = lit2str("expected 'do' or '{' after $0.kind$ condition"),
                        .args = ORERR_ARGS(error_arg_token(start_token), error_arg_token(parser->current)),
                        .show_code_lines = ORERR_LINES(1),
                    ));
                }
            } else {
                unless (consume(parser, TOKEN_THEN)) {
                    parser_error(parser, OR_ERROR(
                        .tag = ERROR_PARSER_EXPECTED_THEN_OR_BRACE_AFTER_BRANCH_CONDITION,
                        .level = ERROR_SOURCE_PARSER,
                        .msg = lit2str("expected 'then' or '{' after $0.kind$ condition"),
                        .args = ORERR_ARGS(error_arg_token(start_token), error_arg_token(parser->current)),
                        .show_code_lines = ORERR_LINES(1),
                    ));
                }
            }

            then_branch = parse_expression(parser);
        }

        token_type_t next_token_check = TOKEN_ERROR;
        switch (branch_type) {
            case BRANCH_TYPE_LOOPING: next_token_check = TOKEN_THEN; break;
            case BRANCH_TYPE_IFTHEN: next_token_check = TOKEN_ELSE; break;
            default: UNREACHABLE();
        }

        unless (match(parser, next_token_check)) {
            else_branch = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(then_branch->end));
        } else {
            else_branch = parse_expression(parser);
        }
    }

    switch (branch_type) {
        case BRANCH_TYPE_LOOPING: {
            ast_node_t *while_ = ast_while(parser->ast, condition, condition_negated, then_branch, else_branch, start_token);
            ASSERT(decl_block, "must");

            if (decl_block->children.count > 0) {
                ast_block_decl(decl_block, ast_statement(parser->ast, while_));
                ast_block_end(decl_block, while_->end);
                while_ = decl_block;
            }

            return while_;
        }
        case BRANCH_TYPE_IFTHEN: {
            ast_node_t *ifthen = ast_ifthen(parser->ast, condition, condition_negated, then_branch, else_branch, start_token);
            ASSERT(decl_block, "must");

            if (decl_block->children.count > 0) {
                ast_block_decl(decl_block, ast_statement(parser->ast, ifthen));
                ast_block_end(decl_block, ifthen->end);
                ifthen = decl_block;
            }
            return ifthen;
        }

        case BRANCH_TYPE_DO: {
            ast_node_t *do_ = ast_do(parser->ast, label, then_branch, else_branch, start_token);
            return do_;
        }
        
        case BRANCH_TYPE_FOR: UNREACHABLE(); break; // handled separately
    }
}

static ast_node_t *parse_for(parser_t *parser) {
    token_t start = parser->previous;

    ast_node_t *decl;
    if (!match(parser, TOKEN_SEMICOLON)) {
        decl = parse_decl(parser, false);
        unless (consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_SEMICOLON,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after the first declaration in a for expression"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    } else {
        decl = &nil_node;
    }

    ast_node_t *cond;
    token_t end_of_check = start;
    if (!match(parser, TOKEN_SEMICOLON)) {
        cond = parse_expression(parser);
        unless(consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_SEMICOLON,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after the condition in a for expression"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
            end_of_check = start;
        } else {
            end_of_check = parser->previous;
        }
    } else {
        cond = &nil_node;
    }

    ast_node_t *increment;
    unless (check(parser, TOKEN_DO) || check(parser, TOKEN_BRACE_OPEN)) {
        increment = parse_expression(parser);
    } else {
        increment = &nil_node;
    }

    ast_node_t *loop = &nil_node;
    if (match(parser, TOKEN_DO)) {
        loop = parse_expression(parser);
    } else if (match(parser, TOKEN_BRACE_OPEN)) {
        loop = parse_block(parser);
    } else {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_EXPRESSION,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected expression for the increment section of a for expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *then;
    if (match(parser, TOKEN_THEN)) {
        then = parse_expression(parser);
    } else {
        then = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(loop->end));
    }

    // true implicit condition if nothing
    if (an_is_none(cond)) {
        cond = ast_implicit_expr(parser->ast, typeid(TYPE_BOOL), WORDU(1), token_implicit_at_start(end_of_check));
    }

    ast_node_t *for_ = ast_for(parser->ast, decl, cond, increment, loop, then, start);

    return for_;
}

static bool is_incoming_function_signature(parser_t* parser) {
    token_t parenthesis_open = parser->previous;
    ASSERT(parenthesis_open.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    lexer_t look_ahead_lexer = parser->lexer;
    s32 parenthesis_level = 1;
    token_t next = parser->current;
    // get matching close parenthesis
    while (true) {
        if (next.type == TOKEN_EOF) {
            return false;
        }

        if (next.type == TOKEN_PARENTHESIS_CLOSE) {
            parenthesis_level--;
        } else if (next.type == TOKEN_PARENTHESIS_OPEN) {
            parenthesis_level++;
        }

        if (parenthesis_level <= 0) {
            break;
        }

        next = lexer_next_token(&look_ahead_lexer);
    }

    next = lexer_next_token(&look_ahead_lexer);

    return next.type == TOKEN_ARROW_RIGHT || next.type == TOKEN_BRACE_OPEN;
}

static bool is_incoming_decl_def(parser_t* parser) {
    lexer_t lookahead_lexer = parser->lexer;
    while (lookahead_lexer.previous_token.type == TOKEN_DIRECTIVE) {
        lexer_next_token(&lookahead_lexer);
    }

    if (lookahead_lexer.previous_token.type != TOKEN_IDENTIFIER) {
        return false;
    }

    token_t token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static ast_node_t *parse_decl_def(parser_t *parser);

static void parse_parameters(parser_t *parser, ast_nodes_t *children) {
    until (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        if (is_incoming_decl_def(parser)) {
            array_push(children, parse_decl_def(parser));
        } else {
            bool inside_type_context = parser->inside_type_context;
            parser->inside_type_context = true;
            array_push(children, parse_expression(parser));
            parser->inside_type_context = inside_type_context;
        }
        if (match(parser, TOKEN_COMMA)) {
            continue;
        }
    }
}

static void parse_function_signature(parser_t *parser, ast_node_t *func_sig) {
    ASSERT(func_sig->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");
    func_sig->children.count = 0;

    array_push(&func_sig->children, &nil_node);
    array_push(&func_sig->children, &nil_node);

    parse_parameters(parser, &func_sig->children);

    unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ')' after the function signature arguments"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    if (match(parser, TOKEN_ARROW_RIGHT)) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = true;
        an_func_def_return(func_sig) = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    } else {
        an_func_def_return(func_sig) = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(func_sig->end));
    }
}

static ast_node_t *parse_function_definition(parser_t *parser) {
    ast_node_t *func_def = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, parser->previous);
    an_func_def_block(func_def) = parse_block(parser);
    func_def->end = parser->previous;

    return func_def;
}

static ast_node_t *parse_array_type(parser_t *parser) {
    ast_node_t *size_expr = &nil_node;

    if (true || !check(parser, TOKEN_BRACKET_CLOSE)) {
        size_expr = parse_expression(parser);
    }

    unless (consume(parser, TOKEN_BRACKET_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_CLOSE_BRACKET_AFTER_SIZE_EXPRESSION,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ']' after the size expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *type_expr = parse_precedence(parser, (prec_t)(PREC_CALL + 1));

    ast_node_t *array_type = ast_array_type(parser->ast, size_expr, type_expr);
    return array_type;
}

static ast_node_t *parse_item_access(parser_t *parser) {
    ast_node_t *access_expr = parse_expression(parser);

    unless (consume(parser, TOKEN_BRACKET_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_CLOSE_BRACKET_AFTER_SIZE_EXPRESSION,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ']' after the size expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *item_access = ast_item_access(parser->ast, &nil_node, access_expr);
    return item_access;
}

static ast_node_t *parse_grouping_or_function_signature_or_definition(parser_t *parser) {
    ast_node_type_t node_type;
    if (is_incoming_function_signature(parser)) {
        node_type = AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE;
    } else  {
        node_type = AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    ast_node_t *expr = ast_node_new(parser->ast, node_type, parser->previous);

    if (node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        parse_function_signature(parser, expr);

        expr->value_type = typeid(TYPE_UNRESOLVED);
        expr->end = parser->previous;

        if (match(parser, TOKEN_BRACE_OPEN)) {
            ast_node_t *definition = parse_function_definition(parser);
            expr = convert_function_definition(parser, expr, definition);
        }
    } else {
        an_operand(expr) = parse_expression(parser);

        unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ')' for group"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        expr->value_type = an_operand(expr)->value_type;
    }

    expr->end = parser->previous;

    return expr;
}

static ast_node_t *parse_call(parser_t *parser) {
    ast_node_t *call = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_CALL, parser->previous);

    an_callee(call) = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(parser->previous));
    call->children.count = an_call_arg_start(call);
    parse_arguments(parser, call);
    call->end = parser->previous;

    return call;
}

static ast_node_t *parse_unary(parser_t *parser) {
    token_t operator = parser->previous;
    ast_node_t *operand = parse_precedence(parser, PREC_UNARY);

    unless (operator.type == TOKEN_PLUS_PLUS || operator.type == TOKEN_MINUS_MINUS) {
        ast_node_t *unary = ast_unary(parser->ast, operator, operand);
        return unary;
    } else {
        token_t equals = operator;
        equals.type= (equals.type == TOKEN_MINUS_MINUS ? TOKEN_MINUS_EQUAL : TOKEN_PLUS_EQUAL);
        equals.view.length = 0;

        ast_node_t *one = ast_implicit_expr(parser->ast, parser->ast->type_set.u64_, WORDU(1), equals);
        one->is_free_number = true;

        ast_node_t *assignment = ast_assignment(parser->ast, operand, one, equals);

        return assignment;
    }
}

static ast_node_t *parse_inferred_type_decl(parser_t *parser) {
    token_t first_token = parser->previous;

    token_t identifier = parser->current;
    unless (consume(parser, TOKEN_IDENTIFIER)) {
        identifier = nil_token;
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_IDENTIFER_AFTER_INFERRED_TYPE_DECL_ANNOTATION,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected identifier after '!'"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *inferred_type_decl = ast_inferred_type_decl(parser->ast, first_token, identifier);
    return inferred_type_decl;
}

static ast_node_t *ast_call_begin(ast_t *ast, ast_node_type_t type, token_t start) {
    ast_node_t *call = ast_node_new(ast, type, start);
    return call;
}

static void ast_call_end(ast_node_t *call, token_t end) {
    call->end = end;
}

static ast_node_t *parse_directive(parser_t *parser) {
    bool allow_multiple_args = false;

    ast_node_t *directive = ast_call_begin(parser->ast, AST_NODE_TYPE_EXPRESSION_DIRECTIVE, parser->previous);
    directive->identifier = parser->previous;

    if (match(parser, TOKEN_PARENTHESIS_OPEN)) {
        allow_multiple_args = true;
    }

    do {
        ast_node_t *arg = parse_expression(parser);
        array_push(&directive->children, arg);

        unless (allow_multiple_args) break;
        unless (match(parser, TOKEN_COMMA)) break;
    } while (true);

    if (allow_multiple_args) {
        unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ')' after the directive call arguments"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    ast_call_end(directive, parser->previous);

    return directive;
}

static ast_node_t *parse_binary(parser_t *parser) {
    token_t operator = parser->previous;

    parse_rule_t *rule = parser_get_rule(operator.type);

    ast_node_t *rhs = parse_precedence(parser, (prec_t)(rule->precedence + 1));

    ast_node_t *binary = ast_binary(parser->ast, operator, &nil_node, rhs);

    return binary;
}

static ast_node_t *parse_cast(parser_t *parser) {
    parse_rule_t *rule = parser_get_rule(parser->previous.type);
    ast_node_t *expr_type = parse_precedence(parser, (prec_t)(rule->precedence+1));
    // ast_node_t *expr_type = parse_precedence(parser, PREC_CAST);

    ast_node_t *cast_node = ast_cast(parser->ast, expr_type, &nil_node);
    
    return cast_node;
}

static ast_node_t *parse_struct_def(parser_t *parser) {
    UNUSED(parser);
    UNREACHABLE();
    return NULL;
}

static ast_node_t *ast_begin_list_initializer(ast_t *ast, token_t open_bracket) {
    ast_node_t *initializer = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST, open_bracket);
    return initializer;
}

static void ast_end_list_initializer(ast_node_t *initializer, token_t close_bracket) {
    initializer->end = close_bracket;
}

static ast_node_t *parse_dot(parser_t *parser) {
    if (match(parser, TOKEN_BRACE_OPEN)) {
        ast_node_t *initiailizer = ast_begin_list_initializer(parser->ast, parser->previous);

        unless (match(parser, TOKEN_BRACE_CLOSE)) {
            do {
                if (check(parser, TOKEN_BRACE_CLOSE)) {
                } else if (check(parser, TOKEN_COMMA)) {
                    ast_node_t *none = &nil_node;
                    array_push(&initiailizer->children, none);
                } else {
                    ast_node_t *argument = parse_expression(parser);
                    array_push(&initiailizer->children, argument);
                }
            } while (match(parser, TOKEN_COMMA));

            unless (consume(parser, TOKEN_BRACE_CLOSE)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_EXPECTED_CLOSE_BRACE,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected '}' after an initialization list"),
                    .args = ORERR_ARGS(error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
        }

        ast_end_list_initializer(initiailizer, parser->previous);

        return initiailizer;
    } else {
        ast_node_t *dot_expression = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_DOT, parser->previous);
        unless (consume(parser, TOKEN_IDENTIFIER)) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSER_EXPECTED_VALID_TOKEN_AFTER_DOT,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected an identifier after '.'"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
        dot_expression->identifier = parser->previous;
        dot_expression->end = parser->current;

        return dot_expression;
    }
}

parse_rule_t rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { parse_grouping_or_function_signature_or_definition,   parse_call,       PREC_CALL },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { parse_block,        NULL,               PREC_BLOCK },
    [TOKEN_BRACE_CLOSE]             = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BRACKET_OPEN]            = { parse_array_type,   parse_item_access,  PREC_CALL },
    [TOKEN_BRACKET_CLOSE]           = { NULL,               NULL,               PREC_NONE },
    [TOKEN_COMMA]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DOT]                     = { parse_dot,          parse_dot,          PREC_CALL },
    [TOKEN_MINUS]                   = { parse_unary,        parse_binary,       PREC_TERM },
    [TOKEN_PLUS]                    = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_STAR]                    = { parse_unary,        parse_binary,       PREC_FACTOR },
    [TOKEN_SLASH]                   = { NULL,               parse_binary,       PREC_FACTOR },
    [TOKEN_COLON]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_PERCENT]                 = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_PERCENT_PERCENT]         = { NULL,               parse_binary,       PREC_TERM },

    [TOKEN_EQUAL]                   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PLUS_EQUAL]              = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_MINUS_EQUAL]             = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_SLASH_EQUAL]             = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_STAR_EQUAL]              = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PERCENT_EQUAL]           = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PERCENT_PERCENT_EQUAL]   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_AND_EQUAL]               = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_OR_EQUAL]                = { NULL,               parse_assignment,   PREC_ASSIGNMENT},

    [TOKEN_BANG]                    = { parse_inferred_type_decl, NULL,         PREC_UNARY },
    [TOKEN_LESS]                    = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER]                 = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_SEMICOLON]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BAR]                     = { NULL,               parse_binary,       PREC_BITWISE_OR },
    [TOKEN_AMPERSAND]               = { parse_unary,        NULL,               PREC_UNARY },
    [TOKEN_SQUIGGLE]                = { NULL,               NULL,               PREC_NONE },
    [TOKEN_PLUS_PLUS]               = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_LESS_LESS]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_AS]                      = { NULL,               parse_cast,         PREC_CAST },
    [TOKEN_EQUAL_EQUAL]             = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_BANG_EQUAL]              = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_LESS_EQUAL]              = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL]           = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_ARROW_RIGHT]             = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DIRECTIVE]               = { parse_directive,    NULL,               PREC_NONE },
    [TOKEN_IDENTIFIER]              = { parse_def_value,    NULL,               PREC_NONE },
    [TOKEN_STRING]                  = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_SYMBOL]                  = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_INTEGER]                 = { parse_number,       NULL,               PREC_NONE },
    [TOKEN_FLOAT]                   = { parse_number,       NULL,               PREC_NONE },
    [TOKEN_ANNOTATION]              = { NULL,               NULL,               PREC_NONE },
    [TOKEN_STRUCT]                  = { parse_struct_def,   NULL,               PREC_NONE },
    [TOKEN_NOT]                     = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_AND]                     = { NULL,               parse_binary,       PREC_AND },
    [TOKEN_OR]                      = { NULL,               parse_binary,       PREC_OR },
    [TOKEN_IF]                      = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_THEN]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_UNLESS]                  = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_WHILE]                   = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_UNTIL]                   = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_FOR]                     = { parse_for,          NULL,               PREC_BLOCK },
    [TOKEN_DO]                      = { parse_branch,       NULL,               PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_TRUE]                    = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_FALSE]                   = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_TYPEOF]                  = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_SIZEOF]                  = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_LEN]                     = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_RETURN]                  = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_BREAK]                   = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_CONTINUE]                = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_EOF]                     = { NULL,               NULL,               PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,               NULL,               PREC_NONE },
};

static bool check_expression(parser_t *parser) {
    return parser_get_rule(parser->current.type)->prefix != NULL;
}

static ast_node_t *parse_precedence(parser_t *parser, prec_t precedence) {
    ParseFn prefix_rule = parser_get_rule(parser->current.type)->prefix;
    ast_node_t *left_operand;

    if (prefix_rule == NULL) {
        left_operand = ast_node_new(parser->ast, AST_NODE_TYPE_NONE, parser->current);
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSEREX_EXPECTED_EXPRESSION,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else {
        advance(parser);

        left_operand = prefix_rule(parser);
    }

    while (precedence <= parser_get_rule(parser->current.type)->precedence) {
        advance(parser);

        ParseFn infix_rule = parser_get_rule(parser->previous.type)->infix;
        ast_node_t *right_operand = infix_rule(parser);

        switch (right_operand->node_type) {
            case AST_NODE_TYPE_EXPRESSION_CAST: {
                ast_node_t *expr = left_operand;
                ast_node_t *cast_type = an_cast_type(right_operand);
                an_cast_expr(right_operand) = expr;
                an_cast_type(right_operand) = cast_type;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
                ast_node_t *accessee = left_operand;
                an_item_accessee(right_operand) = accessee;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_BINARY: {
                an_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
                an_list_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_DOT: {
                an_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_CALL: {
                left_operand = convert_call_expression(left_operand, right_operand);
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
                left_operand = convert_assignment_expression(parser, left_operand, right_operand);
                break;
            }

            default: UNREACHABLE();
        }
    }

    /*
     * I was a little liberal with the parsing of the function signature because I wanted it 
     * to capture both definitions and expressions for the parameters. This allows me
     * to combine a block and a signature to make a definition and then check if all the
     * parameters and definitions. That case is handled already.
     * 
     * Now here, I need to verify that all the parameters are expressions because
     * this is the signature case. I only want the parameters to be expressions of types.
     * 
     * On a special note, this is a good example on why implementing your own parser is
     * good. It allows you to easily make special cases for your grammar. Unless you're,
     * for some reason, focused on making an LL1 parser, don't worry about writing a
     * restrictive grammar.
     */
    if (left_operand->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
            ast_node_t *parameter = left_operand->children.items[i];
            unless (ast_node_type_is_expression(parameter->node_type)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSEREX_EXPECTED_EXPRESSION,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected expression"),
                    .args = ORERR_ARGS(error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                break;
            }
        }
    }

    return left_operand;
}

static parse_rule_t *parser_get_rule(token_type_t type) {
    return &rules[type];
}

static ast_node_t *parse_expression(parser_t *parser) {
    bool inside_type_context = parser->inside_type_context;
    parser->inside_type_context = inside_type_context;

    ast_node_t *expression_node = parse_precedence(parser, parser->inside_type_context ? PREC_OR : PREC_ASSIGNMENT);
    parser->inside_type_context = inside_type_context;

    return expression_node;
}

static ast_node_t *parse_statement(parser_t *parser) {
    ast_node_t *expr = parse_expression(parser);
    ast_node_t *statement_node = ast_statement(parser->ast, expr);
    return statement_node;
}

static ast_node_t *parse_decl_def(parser_t *parser) {
    bool is_intrinsic = false;
    {
        while (match(parser, TOKEN_DIRECTIVE)) {
            if (sv_eq(parser->previous.view, lit2sv("@intrinsic"))) {
                is_intrinsic = true;
            } else {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_INVALID_DECLARATION_DIRECTIVE,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("unknown declaration directive"),
                    .args = ORERR_ARGS(error_arg_token(parser->previous)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                parser->panic_mode = false;
            }
        }
    }

    advance(parser);
    token_t identifier = parser->previous;

    unless (consume(parser, TOKEN_COLON)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSER_EXPECTED_COLON_AFTER_DECLARATION_IDENTIFIER,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ':' after declaration identifier"),
            .args = ORERR_ARGS(error_arg_token(identifier)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *type_expr = NULL;
    if (!check(parser, TOKEN_EQUAL) && !check(parser, TOKEN_COLON)) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = true;
        type_expr = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    } else {
        type_expr = ast_implicit_expr(parser->ast, typeid(TYPE_TYPE), WORDT(typeid(TYPE_UNRESOLVED)), token_implicit_at_end(parser->previous));
    }

    ASSERT(type_expr, "should be set by now");

    bool is_mutable = false;
    bool requires_expression = false;
    if (match(parser, TOKEN_EQUAL)) {
        requires_expression = true;
        is_mutable = true;
    } else if (match(parser, TOKEN_COLON)) {
        requires_expression = true;
        is_mutable = false;
    } else {
        is_mutable = true;
    }

    ast_node_t *init_expr = NULL;
    if (requires_expression) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = false;
        init_expr = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    } else {
        init_expr = ast_nil(parser->ast, typeid(TYPE_UNRESOLVED), token_implicit_at_end(parser->previous));
    }

    ASSERT(init_expr, "should be set by now");

    ast_node_t *decldef = ast_decldef(parser->ast, identifier, type_expr, init_expr);
    decldef->is_mutable = is_mutable;
    decldef->is_intrinsic = is_intrinsic;
    return decldef;
}

static ast_node_t *parse_decl(parser_t *parser, bool is_top_level) {
    ast_node_t *node = NULL;

    if (is_incoming_decl_def(parser)) {
        node = parse_decl_def(parser);
    } else {
        node = parse_statement(parser);
        if (is_top_level) {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSEREX_EXPECTED_DECLARATION,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("only declarations are allowed at the module scope"),
                .args = ORERR_ARGS(error_arg_node(node)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    return node;
}

static void parse_into_module(parser_t *parser, ast_node_t *module) {
    until (match(parser, TOKEN_EOF)) {
        if (is_incoming_decl_def(parser)) {
            ast_node_t *decldef = parse_decl(parser, true);
            array_push(&module->children, decldef);

            unless (consume(parser, TOKEN_SEMICOLON)) {
                parser_error(parser, OR_ERROR(
                    .tag = ERROR_PARSER_EXPECTED_SEMICOLON,
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected ';' after declaration"),
                    .args = ORERR_ARGS(error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
        } else {
            parser_error(parser, OR_ERROR(
                .tag = ERROR_PARSEREX_EXPECTED_DECLARATION,
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected a declaration in the module scope"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
            advance(parser);
        }

        synchronize(parser);
    }

    unless (consume(parser, TOKEN_EOF)) {
        parser_error(parser, OR_ERROR(
            .tag = ERROR_PARSEREX_EXPECTED_EOF_AFTER_MODULE,
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected end-of-file after module"),
            .args = ORERR_ARGS(error_arg_node(module)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }
}

bool parse_string_to_module(ast_t *ast, ast_node_t *module, string_t filepath, string_view_t source) {
    parser_t parser = {0};
    parser_init(&parser, ast, filepath, source);
    
    advance(&parser);

    parse_into_module(&parser, module);

    return !parser.had_error;
}

bool parse(ast_t *ast, string_t file_path, string_view_t source) {
    {
        Nob_String_Builder sb = {0};
        bool success = nob_read_entire_file("./bin/core.odl", &sb);
        if (!success) abort();
        
        string_builder_t sb_ = {.items=sb.items, .count=sb.count};

        string_t core_source = sb_render(&sb_, ast->arena);

        nob_sb_free(sb);

        ast_node_t *core_mod = ast_begin_module(ast);
        parse_string_to_module(ast, core_mod, file_path, string2sv(core_source));
        ast_end_module(core_mod);

        ast_add_module(ast, core_mod, lit2str(CORE_MODULE_NAME));
    }


    ast_node_t *module = ast_begin_module(ast);
    parse_string_to_module(ast, module, file_path, source);
    ast_end_module(module);

    ast_add_module(ast, module, lit2str("program"));

    return ast->errors.count == 0;
}

token_type_t parser_opeq2op(token_type_t type) {
    switch (type) {
    case TOKEN_PLUS_EQUAL: return TOKEN_PLUS;
    case TOKEN_MINUS_EQUAL: return TOKEN_MINUS;
    case TOKEN_STAR_EQUAL: return TOKEN_STAR;
    case TOKEN_SLASH_EQUAL: return TOKEN_SLASH;
    case TOKEN_PERCENT_EQUAL: return TOKEN_PERCENT;
    case TOKEN_PERCENT_PERCENT_EQUAL: return TOKEN_PERCENT_PERCENT;
    case TOKEN_OR_EQUAL: return TOKEN_OR;
    case TOKEN_AND_EQUAL: return TOKEN_AND;

    default: return TOKEN_EQUAL;
    }
}

word_t *ast_multiword_value(ast_t *ast, size_t size_words) {
    word_t z = {.as.u = 0};

    word_t *start = (word_t*)(ast->multiword_data.data + ast->multiword_data.count);
    for (size_t i = 0; i < size_words; ++i) {
        memarr_push(&ast->multiword_data, &z, WORD_SIZE);
    }

    return start;
}

word_t ast_mem2word(ast_t *ast, void *data, type_t type) {
    typedata_t *td = ast_type2td(ast, type);

    switch (td->kind) {
    case TYPE_UNREACHABLE:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNRESOLVED:
    case TYPE_INVALID: return (word_t){0};

    case TYPE_VOID: return (word_t){0};

    case TYPE_BOOL: return (word_t){ .as.u = (*((u8*)data)) };

    case TYPE_STRING: UNREACHABLE(); return (word_t){0}; //todo

    case TYPE_POINTER:
    case TYPE_INTRINSIC_FUNCTION:
    case TYPE_FUNCTION:
    case TYPE_TYPE: return *((word_t*)data);

    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_FLOAT: return td->size == NUM_SIZE_32 ? WORDD(*((f32*)data)) : *((word_t*)data);
        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: return WORDU(*((u8*)data));
            case NUM_SIZE_16: return WORDU(*((u16*)data));
            case NUM_SIZE_32: return WORDU(*((u32*)data));
            case NUM_SIZE_64: return *((word_t*)data);
            }
        }
        }
    }

    case TYPE_STRUCT:
    case TYPE_ARRAY: {
        if (td->size > WORD_SIZE) {
            return WORDP(data);
        } else {
            return *((word_t*)data);
        }
    }

    case TYPE_COUNT: UNREACHABLE(); return (word_t){0};
    }
}

ast_node_val_t zero_value(ast_t *ast, type_t type) {
    if (TYPE_IS_UNRESOLVED(type)) return ast_node_val_word(WORDU(0));

    typedata_t *type_info = ast->type_set.types.items[type.i];
    word_t value = {0};
    if (type_info->size > WORD_SIZE) {
        typedata_t *td = type2typedata(&ast->type_set.types, type);
        word_t *data = ast_multiword_value(ast, b2w(td->size));
        value = WORDP(data);
    }

    ast_node_val_t val = ast_node_val_word(value);
    return val;
}

static void print_indent(u32 level) {
    for (u32 i = 0; i < level; ++i) {
        printf(" ");
    }
}

static void print_line(const cstr_t format, ...) {
	va_list args;
	va_start(args, format);

	vprintf(format, args);

	va_end(args);

	printf("\n");
}

static void ast_print_ast_node(typedatas_t types, ast_node_t *node, u32 level) {
    tmp_arena_t *tmp = allocator_borrow();

    #define type2cstr(node) (type_to_string(types, node->value_type, tmp->allocator).cstr)

    switch (node->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            string_t label = string_format("binary (%.*s): %s", tmp->allocator, node->operator.view.length, node->operator.view.data, type2cstr(node));

            print_indent(level);
            print_line("%s", label.cstr);

            print_indent(level + 1);
            print_line("left");
            ast_print_ast_node(types, an_lhs(node), level+2);

            print_indent(level + 1);
            print_line("right");
            ast_print_ast_node(types, an_rhs(node), level+2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            print_indent(level);
            string_t label = string_format("nil: %s", tmp->allocator, type2cstr(node));
            print_line("%s", label.cstr);
            break;
        }

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {
            print_indent(level);
            string_t label = string_format("inferred type decl: %.*s = %s", tmp->allocator, node->identifier.view.length, node->identifier.view.data, type_to_string(types, node->expr_val.word.as.t, tmp->allocator));
            print_line("%s", label.cstr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            print_indent(level);
            print_line("group (...): %s", type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            print_indent(level);
            print_line("array type: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("size");
            ast_print_ast_node(types, an_array_size_expr(node), level + 2);

            print_indent(level + 1);
            print_line("type");
            ast_print_ast_node(types, an_array_type_expr(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            print_indent(level);
            print_line("item access: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("accessee");
            ast_print_ast_node(types, an_item_accessee(node), level + 2);

            print_indent(level + 1);
            print_line("accessor");
            ast_print_ast_node(types, an_item_accessor(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            print_indent(level);
            print_line("directive(%.*s): %s", node->identifier.view.length, node->identifier.view.data, type2cstr(node));

            print_indent(level + 1);
            print_line("args");
            for (size_t i = an_dir_arg_start(node); i < an_dir_arg_end(node); ++i) {
                ast_node_t *child = node->children.items[i];
                ast_print_ast_node(types, child, level + 2);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            print_indent(level);
            print_line("dot: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("lhs");
            ast_print_ast_node(types, an_list_lhs(node), level+2);
            for (size_t i = an_list_start(node); i < an_list_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                print_indent(level + 1);
                print_line("arg %llu", i);
                if (arg) {
                    ast_print_ast_node(types, arg, level + 2);
                } else {
                    print_indent(level+2);
                    print_line("<default>");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            print_indent(level);
            print_line("cast: %s", type2cstr(node));

            ast_print_ast_node(types, an_rhs(node), level + 1);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            print_indent(level);
            print_line("unary (%.*s): %s", node->operator.view.length, node->operator.view.data, type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            print_indent(level);
            print_line("primary (%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));;
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            print_indent(level);
            print_line("def value (%.*s): %s", node->identifier.view.length, node->identifier.view.data, type2cstr(node));
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            print_indent(level);
            print_line("=: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("lvalue");
            ast_print_ast_node(types, an_lhs(node), level + 2);

            print_indent(level + 1);
            print_line("rhs");
            ast_print_ast_node(types, an_rhs(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            print_indent(level);
            print_line("{...}: %s", type2cstr(node));
            for (size_t i = 0; i < node->children.count; ++i) {
                ast_print_ast_node(types, node->children.items[i], level + 1);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            print_indent(level);
            switch (node->branch_type) {
                case BRANCH_TYPE_DO: {
                    print_line("branch (do:%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));

                    print_indent(level + 1);
                    print_line("do");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }

                case BRANCH_TYPE_FOR: {
                    print_line("branch (for:%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));

                    print_indent(level + 1);
                    print_line("decl");
                    ast_print_ast_node(types, an_for_decl(node), level + 2);

                    print_indent(level + 1);
                    print_line("cond");
                    ast_print_ast_node(types, an_condition(node), level + 2);

                    print_indent(level + 1);
                    print_line("incr");
                    ast_print_ast_node(types, an_for_incr(node), level + 2);

                    print_indent(level + 1);
                    print_line("do");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }

                case BRANCH_TYPE_IFTHEN:
                case BRANCH_TYPE_LOOPING: {
                    cstr_t branch = NULL;
                    if (node->branch_type == BRANCH_TYPE_IFTHEN) {
                        if (node->condition_negated) {
                            branch = "unless";
                        } else {
                            branch = "if";
                        }
                    } else {
                        if (node->condition_negated) {
                            branch = "until";
                        } else {
                            branch = "while";
                        }
                    }

                    print_line("branch (%s): %s", branch, type2cstr(node));

                    print_indent(level + 1);
                    print_line("condition");
                    ast_print_ast_node(types, an_condition(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("else");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            print_indent(level);
            print_line("builtin-call(%.*s): %s", node->identifier.view.length, node->identifier.view.data, type2cstr(node));
            
            print_indent(level+1);
            print_line("arguments");

            for (size_t i = an_bcall_arg_start(node); i < an_bcall_arg_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                ast_print_ast_node(types, arg, level+2);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            print_indent(level);
            print_line("call: %s", type2cstr(node));

            print_indent(level+1);
            print_line("callee");
            ast_print_ast_node(types, an_callee(node), level + 2);
            
            print_indent(level+1);
            print_line("arguments");

            for (size_t i = an_call_arg_start(node); i < an_call_arg_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                ast_print_ast_node(types, arg, level+2);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            print_indent(level);
            print_line("function definition: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("definition");
            ast_print_ast_node(types, an_func_def_block(node), level + 2);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            UNREACHABLE();
            // print_indent(level);
            // print_line("struct: %s", type2cstr(node));

            // print_indent(level + 1);
            // print_line("members");
            // for (size_t i = 0; i < node->as.struct_.declarations.count; ++i) {
            //     ast_node_t *declaration = node->as.struct_.declarations.items[i];
            //     ast_print_ast_node(types, declaration, level + 2);
            // }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            print_indent(level);
            print_line("signature: %s", type2cstr(node));
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            print_indent(level);
            print_line("statement expression: %s", type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            print_indent(level);
            string_view_t label = node->identifier.view;
            if (node->identifier.view.length == 0) {
                label = lit2sv("<none>");
            }
            print_line("%.*s:%.*s", node->start.view.length, node->start.view.data, label.length, label.data);

            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            print_indent(level);
            print_line(".: %s", type2cstr(node));

            print_indent(level+1);
            print_line("item");
            ast_print_ast_node(types, an_lhs(node), level+2);

            print_indent(level + 1);
            print_line("accessor (%.*s)", node->identifier.view.length, node->identifier.view.data);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            print_indent(level);
            print_line("declaration (%.*s): %s", node->identifier.view.length, node->identifier.view.data, type2cstr(node));

            if (an_decl_type(node) != AST_NODE_TYPE_NONE) {
                print_indent(level+1);
                print_line("type expr");
                ast_print_ast_node(types, an_decl_type(node), level+2);
            }

            if (an_decl_expr(node) != AST_NODE_TYPE_NONE) {
                print_indent(level+1);
                print_line("initial value");
                ast_print_ast_node(types, an_decl_expr(node), level+2);
            }
            break;
        }

        case AST_NODE_TYPE_NONE: {
            print_indent(level);
            print_line("<none>");
            break;
        }
    }

    allocator_return(tmp);
}

void ast_print_node(ast_t *ast, ast_node_t *node) {
    ast_print_ast_node(ast->type_set.types, node, 0);
}

void ast_print(ast_t* ast, const char* name) {
    UNUSED(name);

    ast_node_t *module;
    kh_foreach_value(ast->moduleid2node, module, {
        ast_print_ast_node(ast->type_set.types, module, 0);
    });
}
