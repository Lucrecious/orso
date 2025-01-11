#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "error.h"
#include "type.h"
#include "type_set.h"
#include "stringt.h"
#include "tmp.h"

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

#define ERROR_XMACRO(error_type, error_message) [error_type] = error_message,
cstr_t const error_messages[] = {
#include "error.x"
};
#undef ERROR_XMACRO

static int type_hash(type_t id) {
    return kh_int64_hash_func((khint64_t)id.i);
}

static int type_equal_(type_t a, type_t b) {
    return typeid_eq(a, b);
}

implement_table(ptr2sizet, type_t, size_t, type_hash, type_equal_)
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

typedef struct parser_t {
    error_function_t error_fn;
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
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_BITWISE_OR,  // | and type separator
    PREC_UNARY,       // - not &
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef ast_node_t* (*ParseFn)(parser_t*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} parse_rule_t;

void ast_init(ast_t *ast, size_t memory_size_bytes) {
    ast->allocator = (arena_t){0};
    ast->resolved = false;
    ast->root = NULL;

    memarr_init(&ast->constants, memory_size_bytes);

    {
        u64 zero = 0;
        memarr_push(&ast->constants, &zero, sizeof(u64));
    }

    ast->symbols = table_new(s2w, &ast->allocator);

    ast->function_definition_pairs = (fd_pairs_t){.allocator=&ast->allocator};
    type_set_init(&ast->type_set, &ast->allocator);

    ast->builtins = table_new(s2w, &ast->allocator);

    ast->type_to_zero_index = table_new(ptr2sizet, &ast->allocator);
    ast->type_to_creation_node = table_new(type2ns, &ast->allocator);
}

void ast_free(ast_t *ast) {
    arena_free(&ast->allocator);
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
    ast_node_t *node = (ast_node_t*)arena_alloc(&ast->allocator, sizeof(ast_node_t));
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

    node->fold = false;
    node->foldable = false;
    node->fold_level_resolved_at = -1;
    node->is_mutable = false;
    node->identifier = nil_token;
    node->ref_decl = &nil_node;

    node->jmp_nodes.allocator = &ast->allocator;
    node->jmp_out_scope_node = &nil_node;

    node->lvalue_node = &nil_node;
    node->value_index = value_index_nil();

    node->last_statement = &nil_node;

    node->children = (ast_nodes_t){.allocator=&ast->allocator};

    switch (node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

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
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_CALL: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_UNARY:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_EXPRESSION_JMP:
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            node->as.initiailizer.type = NULL;
            node->as.initiailizer.arguments = (ast_nodes_t){.allocator=&ast->allocator};
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            node->as.struct_.declarations = (ast_nodes_t){.allocator=&ast->allocator};
            break;
        }
        
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

ast_node_t *ast_nil(ast_t *ast, type_t value_type, token_t token_location) {
    ast_node_t *nil_node = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_NIL, token_location);
    nil_node->value_type = value_type;
    nil_node->value_index = zero_value(ast, value_type);
    return nil_node;
}

static ast_node_t *ast_primaryi(ast_t *ast, i64 value, ast_node_t extra_params) {
    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, extra_params.start);

    primary->value_index = memarr_push_value(&ast->constants, &value, sizeof(i64));
    primary->value_type = ast->type_set.i64_;

    return primary;
}

static ast_node_t *ast_primaryf(ast_t *ast, f64 value, ast_node_t extra_params) {
    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, extra_params.start);

    primary->value_index = memarr_push_value(&ast->constants, &value, sizeof(f64));
    primary->value_type = ast->type_set.f64_;

    return primary;
}

static ast_node_t *ast_primaryb(ast_t *ast, bool value, ast_node_t extra_params) {
    ast_node_t *primary = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, extra_params.start);

    byte byte_value = (byte)value;

    primary->value_index = memarr_push_value(&ast->constants, &byte_value, sizeof(byte));
    primary->value_type = typeid(TYPE_BOOL);

    return primary;
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

static ast_node_t *ast_def_value(ast_t *ast, token_t identifier) {
    ast_node_t *def_value = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_DEF_VALUE, identifier);
    def_value->identifier = identifier;
    return def_value;
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

static void parser_init(parser_t *parser, ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn) {
    lexer_init(&parser->lexer, file_path, source);
    parser->ast = ast;
    parser->error_fn = error_fn;
    parser->had_error = false;
    parser->panic_mode = false;
    parser->inside_type_context = false;
}

static void parser_error(parser_t *parser, error_t error) {
    parser->had_error = true;
    parser->panic_mode = true;
    if (parser->error_fn) parser->error_fn(parser->ast, error);
}

static void advance(parser_t *parser) {
    parser->previous = parser->current;
    parser->current = lexer_next_token(&parser->lexer);
}

static void consume(parser_t *parser, token_type_t type) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }

    error_t error = make_error_token(ERROR_PARSER_UNEXPECTED_TOKEN, parser->current, type);
    parser_error(parser, error);
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

    while (!check(parser, TOKEN_EOF)) {
        switch (parser->current.type) {
            case TOKEN_SEMICOLON:
            case TOKEN_BRACE_CLOSE:
            case TOKEN_PARENTHESIS_CLOSE:
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
static ast_node_t* parse_precedence(parser_t* parser, Precedence precedence);

bool ast_node_type_is_decl_or_stmt(ast_node_type_t node_type) {
    switch (node_type) {
        case AST_NODE_TYPE_EXPRESSION_CASE:
        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_MODULE:
            return false;
        
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
            return true;
    }
}

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

token_t token_implicit_at_end(token_t token) {
    token.view.data += token.view.length;
    token.loc.column += token.view.length;
    token.view.length = 0;
    return token;
}

value_index_t memarr_push_value(memarr_t *arr, void *data, size_t size_bytes) {
    size_t index = memarr_push(arr, data, size_bytes);
    value_index_t vi = value_index_(index);
    return vi;
}

static ast_node_t *parse_number(parser_t *parser) {
    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {

            i64 value = cstrn_to_i64(parser->previous.view.data, parser->previous.view.length);

            ast_node_t *primary = ast_primaryi(parser->ast, value, (ast_node_t){
                .start = parser->previous,
            });

            return primary;
        }

        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.view.data, parser->previous.view.length);

            ast_node_t *primary = ast_primaryf(parser->ast, value, (ast_node_t){
                .start = parser->previous,
            });

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
            consume(parser, TOKEN_IDENTIFIER);
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
            parser_error(parser, make_error_node(ERROR_PARSER_EXPECTED_DECLARATION, parameter));
            left_operand->node_type = AST_NODE_TYPE_NONE;
            return left_operand;
        }
    }

    ast_node_t *block = an_func_def_block(func_def);

    func_def->children.count = 0;

    for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
        array_push(&func_def->children, left_operand->children.items[i]);
    }

    array_push(&func_def->children, an_func_def_return(left_operand));
    array_push(&func_def->children, block);

    // prevents freeing the node  parameter and return expressions
    left_operand->node_type = AST_NODE_TYPE_NONE;

    return func_def;
}

static ast_node_t *parse_assignment(parser_t *parser) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_ASSIGNMENT, parser->previous);

    ast_node_t *rhs = parse_expression(parser);
    an_rhs(expression_node) = rhs;
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_def_value(parser_t *parser) {
    ast_node_t *def_value = ast_def_value(parser->ast, parser->previous);
    return def_value;
}

static ast_node_t *parse_block(parser_t *parser) {
    ast_node_t *block = ast_block_begin(parser->ast, parser->previous);

    until (check(parser, TOKEN_BRACE_CLOSE) || check(parser, TOKEN_EOF)) {
        ast_node_t *decl_node = parse_decl(parser, false);

        consume(parser, TOKEN_SEMICOLON);

        ast_block_decl(block, decl_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE);

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

    token_t label = nil_token;


    if (branch_token_type == TOKEN_DO) {
        branch_type = BRANCH_TYPE_DO;

        if (match(parser, TOKEN_COLON)) {
            consume(parser, TOKEN_IDENTIFIER);
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

        condition = parse_expression(parser);
        if (match(parser, TOKEN_BRACE_OPEN)) {
            then_branch= parse_block(parser);
        } else {
            if (branch_type == BRANCH_TYPE_LOOPING) {
                consume(parser, TOKEN_DO);
            } else {
                consume(parser, TOKEN_THEN);
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
            return while_;
        }
        case BRANCH_TYPE_IFTHEN: {
            ast_node_t *ifthen = ast_ifthen(parser->ast, condition, condition_negated, then_branch, else_branch, start_token);
            return ifthen;
        }

        case BRANCH_TYPE_DO: {
            ast_node_t *do_ = ast_do(parser->ast, label, then_branch, else_branch, start_token);
            return do_;
        }
    }
}

static bool is_incoming_function_signature(parser_t* parser) {
    ASSERT(parser->previous.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    lexer_t look_ahead_lexer = parser->lexer;
    i32 parenthesis_level = 1;
    token_t next = parser->current;
    // get matching close parenthesis
    while (true) {
        if (next.type == TOKEN_EOF) {
            error_t error = make_error_token(ERROR_PARSER_UNEXPECTED_TOKEN, next, TOKEN_PARENTHESIS_CLOSE);
            parser_error(parser, error);
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
    if (parser->current.type != TOKEN_IDENTIFIER) {
        return false;
    }

    lexer_t lookahead_lexer = parser->lexer;

    token_t token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static ast_node_t *parse_decl_def(parser_t *parser);

static void parse_parameters(parser_t *parser, ast_nodes_t *children) {
    ASSERT(children->count == 0, "must be cleared first");

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

    if (children->count == 0) {
        array_push(children, ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(parser->previous)));
    }
}

static void parse_function_signature(parser_t *parser, ast_node_t *func_sig) {
    ASSERT(func_sig->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");
    func_sig->children.count = 0;

    parse_parameters(parser, &func_sig->children);

    array_push(&func_sig->children, &nil_node);
    array_push(&func_sig->children, &nil_node);

    consume(parser, TOKEN_PARENTHESIS_CLOSE);

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

static ast_node_t *parse_grouping_or_function_signature_or_definition(parser_t *parser) {
    ast_node_type_t node_type;
    if (is_incoming_function_signature(parser)) {
        node_type = AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE;
    } else  {
        node_type = AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    ast_node_t* expression_node = ast_node_new(parser->ast, node_type, parser->previous);

    if (node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        parse_function_signature(parser, expression_node);

        expression_node->value_type = typeid(TYPE_UNRESOLVED);
        expression_node->end = parser->previous;

        if (match(parser, TOKEN_BRACE_OPEN)) {
            ast_node_t *definition = parse_function_definition(parser);
            expression_node = convert_function_definition(parser, expression_node, definition);
        }
    } else {
        an_operand(expression_node) = parse_expression(parser);

        consume(parser, TOKEN_PARENTHESIS_CLOSE);

        expression_node->value_type = an_operand(expression_node)->value_type;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static void parse_arguments(parser_t* parser, ast_node_t *parent) {
    size_t parameter_count = 0;
    unless (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            ast_node_t *argument = parse_expression(parser);
            array_push(&parent->children, argument);
            ++parameter_count;
        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_PARENTHESIS_CLOSE);

    if (parameter_count > MAX_PARAMETERS - 1) {
        error_t error = make_error_node(ERROR_PARSER_TOO_MANY_PARAMETERS, parent);
        parser_error(parser, error);
    }
}

static ast_node_t *parse_call(parser_t *parser) {
    ast_node_t *call = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_CALL, parser->previous);

    an_callee(call) = ast_nil(parser->ast, typeid(TYPE_VOID), token_implicit_at_end(parser->previous));
    call->children.count = an_call_arg_start(call);
    parse_arguments(parser, an_callee(call));
    call->end = parser->previous;

    return call;
}

static ast_node_t *parse_unary(parser_t *parser) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_UNARY, parser->previous);
    expression_node->operator = parser->previous;

    an_operand(expression_node) = parse_precedence(parser, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_binary(parser_t *parser) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_BINARY, parser->previous);

    token_t operator = parser->previous;
    expression_node->operator = operator;

    parse_rule_t *rule = parser_get_rule(operator.type);

    expression_node->start = parser->previous;

    ast_node_t *rhs = parse_precedence(parser, (Precedence)(rule->precedence + 1));
    an_rhs(expression_node) = rhs;
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_struct_def(parser_t *parser) {
    ast_node_t *struct_definition = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION, parser->previous);

    consume(parser, TOKEN_BRACE_OPEN);

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        ast_node_t *declaration_node = parse_decl_def(parser);
        array_push(&struct_definition->children, declaration_node);
        consume(parser, TOKEN_SEMICOLON);
    }

    consume(parser, TOKEN_BRACE_CLOSE);

    struct_definition->end = parser->previous;

    return struct_definition;
}

static ast_node_t *parse_dot(parser_t* parser) {
    if (match(parser, TOKEN_BRACE_OPEN)) {
        ast_node_t *initiailizer = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER, parser->previous);
        if (!match(parser, TOKEN_BRACE_CLOSE)) {
            do {
                if (check(parser, TOKEN_COMMA) || check(parser, TOKEN_BRACE_CLOSE)) {
                    array_push(&initiailizer->as.initiailizer.arguments, NULL);
                } else {
                    ast_node_t *argument = parse_expression(parser);
                    array_push(&initiailizer->as.initiailizer.arguments, argument);
                }

            } while (match(parser, TOKEN_COMMA));

            consume(parser, TOKEN_BRACE_CLOSE);
        }

        initiailizer->end = parser->previous;

        return initiailizer;
    } else {
        ast_node_t *dot_expression = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_DOT, parser->previous);
        consume(parser, TOKEN_IDENTIFIER);
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
    [TOKEN_BRACKET_OPEN]            = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BRACKET_CLOSE]           = { NULL,               NULL,               PREC_NONE },
    [TOKEN_COMMA]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DOT]                     = { NULL,               parse_dot,          PREC_CALL },
    [TOKEN_MINUS]                   = { parse_unary,        parse_binary,       PREC_TERM },
    [TOKEN_PLUS]                    = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_STAR]                    = { NULL,               parse_binary,       PREC_FACTOR },
    [TOKEN_SLASH]                   = { NULL,               parse_binary,       PREC_FACTOR },
    [TOKEN_COLON]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_PERCENT]                 = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_PERCENT_PERCENT]         = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_EQUAL]                   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_BANG]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_LESS]                    = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER]                 = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_SEMICOLON]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BAR]                     = { NULL,               parse_binary,       PREC_BITWISE_OR },
    [TOKEN_AMPERSAND]               = { parse_unary,        NULL,               PREC_UNARY },
    [TOKEN_PLUS_PLUS]               = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_EQUAL_EQUAL]             = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_BANG_EQUAL]              = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_LESS_EQUAL]              = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL]           = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_ARROW_RIGHT]             = { NULL,               NULL,               PREC_NONE },
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
    [TOKEN_FOR]                     = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DO]                      = { parse_branch,       NULL,               PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_TRUE]                    = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_FALSE]                   = { parse_literal,      NULL,               PREC_NONE },
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

static ast_node_t *parse_precedence(parser_t *parser, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = parser_get_rule(parser->previous.type)->prefix;
    ast_node_t *left_operand;

    if (prefix_rule == NULL) {
        left_operand = ast_node_new(parser->ast, AST_NODE_TYPE_NONE, parser->previous);
        error_t error = make_error_node(ERROR_PARSER_EXPECTED_EXPRESSION, left_operand);
        parser_error(parser, error);
    } else {
        left_operand = prefix_rule(parser);
    }

    while (precedence <= parser_get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = parser_get_rule(parser->previous.type)->infix;
        ast_node_t *right_operand = infix_rule(parser);

        switch (right_operand->node_type) {
            case AST_NODE_TYPE_EXPRESSION_BINARY: {
                an_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
                right_operand->as.initiailizer.type = left_operand;
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
                parser_error(parser, make_error_node(ERROR_PARSER_EXPECTED_TYPE, parameter));
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
    bool fold = false;
    bool is_type_directive = false;
    if (match(parser, TOKEN_DIRECTIVE)) {
        token_t directive = parser->previous;
        fold = (directive.view.length - 1 == strlen("fold") && strncmp(directive.view.data + 1, "fold", 4) == 0);

        is_type_directive = (directive.view.length - 1 == strlen("type") && strncmp(directive.view.data + 1, "type", 4) == 0);
    }

    bool inside_type_context = parser->inside_type_context;
    parser->inside_type_context = is_type_directive || inside_type_context;

    ast_node_t *expression_node = parse_precedence(parser, parser->inside_type_context ? PREC_OR : PREC_ASSIGNMENT);
    expression_node->fold = fold;

    parser->inside_type_context = inside_type_context;

    return expression_node;
}

static ast_node_t *parse_statement(parser_t *parser) {
    ast_node_t *statement_node = ast_node_new(parser->ast, AST_NODE_TYPE_DECLARATION_STATEMENT, parser->current);
    an_expression(statement_node) = parse_expression(parser);

    statement_node->end = parser->previous;

    return statement_node;
}

static ast_node_t *parse_decl_def(parser_t *parser) {
    ast_node_t *definition_node = ast_node_new(parser->ast, AST_NODE_TYPE_DECLARATION_DEFINITION, parser->current);

    advance(parser);

    definition_node->value_type = typeid(TYPE_UNRESOLVED);

    definition_node->value_index = value_index_nil();

    definition_node->identifier = parser->previous;

    consume(parser, TOKEN_COLON);

    if (!check(parser, TOKEN_EQUAL) && !check(parser, TOKEN_COLON)) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = true;
        an_decl_type(definition_node) = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    }

    // TODO: try to do constant vs variable detection a little more clever...
    bool requires_expression = false;
    if (match(parser, TOKEN_EQUAL)) {
        requires_expression = true;
        definition_node->is_mutable = true;
    } else if (match(parser, TOKEN_COLON)) {
        requires_expression = true;
    }

    if (requires_expression) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = false;
        an_decl_expr(definition_node) = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    }

    definition_node->end = parser->previous;
    return definition_node;
}

static ast_node_t *parse_decl(parser_t *parser, bool is_top_level) {
    ast_node_t *node = NULL;
    if (is_incoming_decl_def(parser)) {
        node = parse_decl_def(parser);
    } else {
        node = parse_statement(parser);
        if (is_top_level) {
            parser_error(parser, make_error_node(ERROR_PARSER_EXPECTED_DECLARATION, node));
            advance(parser);
        }
    }

    if (parser->panic_mode) {
        synchronize(parser);
    }

    return node;
}

bool parse_expr(ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn) {
    parser_t parser = {0};
    parser_init(&parser, ast, file_path, source, error_fn);

    advance(&parser);

    parser.inside_type_context = false;
    ast->root = parse_precedence(&parser, PREC_BLOCK);

    consume(&parser, TOKEN_EOF);

    return !parser.had_error;
}

bool parse(ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn) {
    parser_t parser = {0};
    parser_init(&parser, ast, file_path, source, error_fn);

    advance(&parser);

    ast->root = ast_node_new(parser.ast, AST_NODE_TYPE_EXPRESSION_BLOCK, parser.previous);

    while (!match(&parser, TOKEN_EOF)) {
        ast_node_t *declaration_node = parse_decl_def(&parser);
        array_push(&ast->root->children, declaration_node);
        consume(&parser, TOKEN_SEMICOLON);
    }

    consume(&parser, TOKEN_EOF);

    return !parser.had_error;
}

value_index_t add_value_to_ast_constant_stack(ast_t *ast, void *data, type_t type) {
    type_info_t *type_info = get_type_info(&ast->type_set.types, type);
    size_t size = type_info->size;
    return memarr_push_value(&ast->constants, data, size);
}

value_index_t zero_value(ast_t *ast, type_t type) {
    size_t result = 0;
    if (table_get(ptr2sizet, ast->type_to_zero_index, type, &result)) {
        return value_index_(result);
    }

    type_info_t *type_info = ast->type_set.types.items[type.i];

    word_t value[bytes_to_words(type_info->size)];

    switch (type_info->kind) {
        case TYPE_POINTER:
        case TYPE_BOOL: value[0] = WORDI(0); break;
        
        case TYPE_NUMBER: {
            switch (type_info->data.num) {
                case NUM_TYPE_SIGNED:
                case NUM_TYPE_UNSIGNED: value[0] = WORDI(0); break;
                case NUM_TYPE_FLOAT: value[0] = WORDD(0); break;
            }
            break;
        }

        case TYPE_STRING:
            UNREACHABLE();
            break;

        case TYPE_STRUCT: {
            for (size_t i = 0; i < bytes_to_words(type_info->size); i++) {
                value[i] = WORDI(0);
            }
            break;
        }

        case TYPE_LABEL: {
            UNREACHABLE();
            break;
        }

        case TYPE_COUNT:
        case TYPE_UNDEFINED:
        case TYPE_UNREACHABLE:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_TYPE:
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
            UNREACHABLE();
            value[0] = WORDI(0);
            break;

        case TYPE_VOID: break;
    }

    value_index_t zero_index = add_value_to_ast_constant_stack(ast, value, type);
    table_put(ptr2sizet, ast->type_to_zero_index, type, zero_index.index);
    return zero_index;
}

type_t get_folded_type(ast_t *ast, value_index_t index) {
    ASSERT(index.exists, "must be there");

    type_t type;
    if (memarr_get(&ast->constants, index.index, sizeof(type_t), &type)) {
        return type;
    }

    return typeid(TYPE_INVALID);
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

static void ast_print_ast_node(type_infos_t types, ast_node_t *node, u32 level) {
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
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            print_indent(level);
            print_line("group (...): %s", type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            print_indent(level);
            print_line("type initializer: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("type");
            ast_print_ast_node(types, node->as.initiailizer.type, level+2);
            for (size_t i = 0; i < node->as.initiailizer.arguments.count; ++i) {
                ast_node_t *arg = node->as.initiailizer.arguments.items[i];
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

        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            print_indent(level);
            print_line("implicit cast: %s", type2cstr(node));

            ast_print_ast_node(types, an_operand(node), level + 1);
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
            print_indent(level);
            print_line("struct: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("members");
            for (size_t i = 0; i < node->as.struct_.declarations.count; ++i) {
                ast_node_t *declaration = node->as.struct_.declarations.items[i];
                ast_print_ast_node(types, declaration, level + 2);
            }
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

void ast_print(ast_t* ast, const char* name) {
    printf("=== %s ===\n", name);
    ast_print_ast_node(ast->type_set.types, ast->root, 0);
}
