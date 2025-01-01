#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "common.h"
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

cstr_t const error_messages[] = {
    [ERROR_PARSER_UNEXPECTED_TOKEN] = "unexpected token",
    [ERROR_PARSER_EXPECTED_OPEN_BRACE] = "expected open brace: '{'",
    [ERROR_PARSER_EXPECTED_CLOSE_BRACE] = "expected close brace: '}'",
    [ERROR_PARSER_EXPECTED_DO_OR_BLOCK] = "expected 'do' or block",
    [ERROR_PARSER_EXPECTED_THEN_OR_BLOCK] = "expected 'then' or block",
    [ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS] = "expected close parenthesis: ')'",
    [ERROR_PARSER_EXPECTED_SEMICOLON] = "expected semicolon: ';'",
    [ERROR_PARSER_EXPECTED_DECLARATION_COLON] = "expected colon: ':'",
    [ERROR_PARSER_EXPECTED_DECLARATION] = "expected declaration",
    [ERROR_PARSER_EXPECTED_EXPRESSION] = "expected expression",
    [ERROR_PARSER_EXPECTED_EOF] = "expected end of file",
    [ERROR_PARSER_DOT_OPERATOR_ONLY_NAMES_FOR_NOW] = "dot operator only valid for names right now",
    [ERROR_PARSER_TOO_MANY_PARAMETERS] = "too many parameters; the max is 100",

    [ERROR_ANALYSIS_INVALID_UNARY_OPERAND] = "unary operation cannot be performed without an explicit cast",
    [ERROR_ANALYSIS_INVALID_BINARY_OPERANDS] = "binary operation cannot be performed without an explicit cast",
    [ERROR_ANALYSIS_INVALID_MEMBER_ACCESS] = "expression is does not have accessible members",
    [ERROR_ANALYSIS_INVALID_RETURN_TYPE] = "expression must resolve to a type for return",
    [ERROR_ANALYSIS_EXPECTED_CONSTANT] = "expression must be resolved by compile time",
    [ERROR_ANALYSIS_EXPECTED_LVALUE] = "expression must resolve to an lvalue",
    [ERROR_ANALYSIS_EXPECTED_CALLABLE] = "expression must be callable",
    [ERROR_ANALYSIS_EXPECTED_RESOLVED] = "expression must be defined and resolved",
    [ERROR_ANALYSIS_EXPECTED_TYPE] = "expression must be a type",
    [ERROR_ANALYSIS_TYPE_MISMATCH] = "type mismatch; expression requires explicit cast",
    [ERROR_ANALYSIS_CANNOT_RETURN_INSIDE_RETURN] = "cannot return inside return",
    [ERROR_ANALYSIS_CANNOT_ACCESS_MUTABLE_ON_DIFFERENT_FOLD_LEVEL] = "cannot access mutable definition on a different fold level",
    [ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION] = "cannot overload definition",
    [ERROR_ANALYSIS_CANNOT_FOLD_ON_ERRORED_FUNCTION_DEFINITION] = "cannot fold on errored function definition",
    [ERROR_ANALYSIS_FOLDING_LOOP] = "folding operation causes a dependency loop",
    [ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES] = "function must return on all branches",
    [ERROR_ANALYSIS_MEMBER_DOES_NOT_EXIST] = "member does not exist",
    [ERROR_ANALYSIS_INVALID_TYPE_FOR_TYPE_INITIALIZER_LIST] = "invalid type for type initializer list",
    [ERROR_ANALYSIS_TOO_MANY_STRUCT_ARGUMENTS] = "too many struct arguments",

    [ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG] = "memory size is too large to index. the max is 2^32",
};

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
    ast_t* ast; // TODO: Consider moving this outside parser and instead passed through arguments
    lexer_t lexer;
    token_t previous;
    token_t current;

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

typedef ast_node_t* (*ParseFn)(parser_t*, bool inside_type_context);

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
        size_t _unused;
        memarr_push(&ast->constants, &zero, (sizeof(u64)), &_unused);
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
    .return_guarentee = RETURN_GUARENTEE_NONE,
    .lvalue_node = &nil_node,
};

ast_node_t *ast_node_new(ast_t *ast, ast_node_type_t node_type, bool inside_type_context, token_t start) {
    ast_node_t *node = (ast_node_t*)arena_alloc(&ast->allocator, sizeof(ast_node_t));
    
    node->node_type = node_type;
    node->start = start;
    node->end = start;
    node->operator = start;
    node->return_guarentee = RETURN_GUARENTEE_NONE;
    node->value_type.i = TYPE_UNRESOLVED;
    node->condition_negated = false;
    node->looping = false;
    node->requires_tmp_for_cgen = true;


    node->inside_type_context = inside_type_context;

    node->fold = false;
    node->foldable = false;
    node->fold_level_resolved_at = -1;
    node->is_mutable = false;
    node->identifier = nil_token;
    node->ref_decl = &nil_node;

    node->lvalue_node = NULL;
    node->value_index = value_index_nil();

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
            node->as.call.callee = NULL;
            node->as.call.arguments = (ast_nodes_t){.allocator=&ast->allocator};
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_UNARY:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_EXPRESSION_RETURN:
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
            node->as.function.block = NULL;
            node->as.function.compilable = false;
            node->as.function.parameter_nodes = (ast_nodes_t){.allocator=&ast->allocator};
            node->as.function.return_type_expression = NULL;
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

static void parser_init(parser_t *parser, ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn) {
    lexer_init(&parser->lexer, file_path, source);
    parser->ast = ast;
    parser->error_fn = error_fn;
    parser->had_error = false;
    parser->panic_mode = false;
}

static void error_at(parser_t *parser, token_t *token, error_type_t error_type) {
    if (parser->panic_mode) {
        return;
    }
    parser->panic_mode = true;
    parser->had_error = true;

    if (!parser->error_fn) {
        return;
    }

    error_t error = {
        .type = error_type,
        .region_type = ERROR_REGION_TYPE_TOKEN,
        .first = *token,
    };

    parser->error_fn(error);
}

static void error_at_current(parser_t *parser, error_type_t error_type) {
    error_at(parser, &parser->current, error_type);
}

static void error(parser_t *parser, error_type_t error_type) {
    error_at(parser, &parser->previous, error_type);
}

static void advance(parser_t *parser) {
    parser->previous = parser->current;

    for (;;) {
        parser->current = lexer_next_token(&parser->lexer);
        if (parser->current.type != TOKEN_ERROR) {
            break;
        }

        error_at_current(parser, ERROR_PARSER_UNEXPECTED_TOKEN); 
    }
}

static void consume(parser_t *parser, token_type_t type, error_type_t error_type) {
    if (parser->current.type == type) {
        advance(parser);
        return;
    }

    error_at_current(parser, error_type);
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
        if (parser->previous.type == TOKEN_SEMICOLON) {
            return;
        }

        switch (parser->current.type) {
            case TOKEN_BRACE_CLOSE:
            case TOKEN_PARENTHESIS_CLOSE:
                return;
            
            default: break;
        }

        advance(parser);
    }
}

static ast_node_t *parse_declaration(parser_t* parser, bool is_top_level);
static ast_node_t *parse_expression(parser_t* parser, bool inside_type_context);
static ast_node_t *parse_statement(parser_t* parser, bool lack_semicolin);
static parse_rule_t *parser_get_rule(token_type_t type);
static bool check_expression(parser_t* parser);
static ast_node_t* parse_precedence(parser_t* parser, bool inside_type_context, Precedence precedence);

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

bool memarr_push_value(memarr_t *arr, void *data, size_t size_bytes, value_index_t *out_index) {
    size_t index = 0;
    if (memarr_push(arr, data, size_bytes, &index)) {
        *out_index = value_index_(index);
        return true;
    }

    *out_index = value_index_nil();
    return false;
}

static ast_node_t *parse_number(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, inside_type_context, parser->previous);

    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            i64 value = cstrn_to_i64(parser->previous.view.data, parser->previous.view.length);
            expression_node->value_type = parser->ast->type_set.i64_;
            unless (memarr_push_value(&parser->ast->constants, &value, sizeof(i64), &expression_node->value_index)) {
                error(parser, ERROR_CODEGEN_NOT_ENOUGH_MEMORY);
            }
            break;
        }
        case TOKEN_FLOAT: {
            f64 value = cstrn_to_f64(parser->previous.view.data, parser->previous.view.length);
            expression_node->value_type = parser->ast->type_set.f64_;
            unless (memarr_push_value(&parser->ast->constants, &value, sizeof(f64), &expression_node->value_index)) {
            }
            break;
        }
        default: UNREACHABLE();
    }

    expression_node->foldable = true;

    return expression_node;
}

static ast_node_t *parse_return(parser_t *parser, bool inside_type_context) {
    ast_node_t *return_expr = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_RETURN, inside_type_context, parser->previous);
    if (check_expression(parser)) {
        an_expression(return_expr) = parse_expression(parser, inside_type_context);
        return_expr->end = an_expression(return_expr)->end;
    }

    return_expr->value_type = typeid(TYPE_UNDEFINED);

    return return_expr;
}

static ast_node_t *parse_literal(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_PRIMARY, inside_type_context, parser->previous);

    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            expression_node->value_type = typeid(TYPE_BOOL);

            bool is_true = parser->previous.type == TOKEN_TRUE;
            byte value = (byte)is_true;
            unless (memarr_push_value(&parser->ast->constants, &value, sizeof(byte), &expression_node->value_index)) {
                error(parser, ERROR_CODEGEN_NOT_ENOUGH_MEMORY);
            }
            break;
        }
        case TOKEN_NULL: {
            expression_node->value_type = typeid(TYPE_VOID);
            expression_node->value_index = value_index_(0);
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
        default:
            UNREACHABLE();
    }

    expression_node->foldable = true;

    return expression_node;
}

static ast_node_t *convert_assignment_expression(parser_t *parser, ast_node_t *left, ast_node_t *assignment) {
    (void)parser;
    assignment->start = left->start;
    an_lhs(assignment) = left;
    return assignment;
}

static ast_node_t *convert_call_expression(ast_node_t *left_operand, ast_node_t *call) {
    call->start = left_operand->start;
    call->as.call.callee = left_operand;
    return call;
}

static ast_node_t *convert_function_definition(parser_t *parser, ast_node_t *left_operand, ast_node_t *function_definition) {
    for (size_t i = 0; i < left_operand->as.function.parameter_nodes.count; i++) {
        ast_node_t *parameter = left_operand->as.function.parameter_nodes.items[i];
        if (parameter->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            error_at(parser, &parameter->start, ERROR_PARSER_EXPECTED_DECLARATION);
            left_operand->node_type = AST_NODE_TYPE_NONE;
            return left_operand;
        }
    }

    function_definition->as.function.parameter_nodes = left_operand->as.function.parameter_nodes;
    function_definition->as.function.return_type_expression = left_operand->as.function.return_type_expression;

    // prevents freeing the node  parameter and return expressions
    left_operand->node_type = AST_NODE_TYPE_NONE;

    return function_definition;
}

static ast_node_t *parse_assignment(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_ASSIGNMENT, inside_type_context, parser->previous);

    ast_node_t *rhs = parse_expression(parser, inside_type_context);
    an_rhs(expression_node) = rhs;
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_def_value(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_DEF_VALUE, inside_type_context, parser->previous);
    expression_node->identifier = parser->previous;
    return expression_node;
}

static void parse_block_(parser_t *parser, ast_node_t *block) {
    block->return_guarentee = RETURN_GUARENTEE_NONE;

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        ast_node_t *declaration_node = parse_declaration(parser, false);
        array_push(&block->children, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_BRACE);
}

static ast_node_t *parse_block(parser_t *parser, bool inside_type_context) {
    (void)inside_type_context;

    ast_node_t* expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_BLOCK, inside_type_context, parser->previous);

    parse_block_(parser, expression_node);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_ifelse(parser_t *parser, bool inside_type_context) {
    ast_node_t* expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_BRANCHING, inside_type_context, parser->previous);

    expression_node->condition_negated = false;
    if (parser->previous.type == TOKEN_UNLESS || parser->previous.type == TOKEN_UNTIL) {
        expression_node->condition_negated = true;
    }

    expression_node->looping = false;
    if (parser->previous.type == TOKEN_WHILE || parser->previous.type == TOKEN_UNTIL) {
        expression_node->looping = true;
    }

    expression_node->return_guarentee = RETURN_GUARENTEE_NONE;

    an_condition(expression_node) = parse_expression(parser, inside_type_context);
    if (match(parser, TOKEN_BRACE_OPEN)) {
        an_then(expression_node) = parse_block(parser, inside_type_context);
    } else {
        if (expression_node->looping) {
            consume(parser, TOKEN_DO, ERROR_PARSER_EXPECTED_DO_OR_BLOCK);
        } else {
            consume(parser, TOKEN_THEN, ERROR_PARSER_EXPECTED_THEN_OR_BLOCK);
        }

        {
            ast_node_t *then_expression = parse_expression(parser, inside_type_context);
            an_then(expression_node) = then_expression;
        }
    }

    if (!match(parser, TOKEN_ELSE)) {
        return expression_node;
    }

    {
        // ast_node_t* else_statement = statement(parser, true);
        // ast_node_t* else_expression = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_STATEMENT, inside_type_context, else_statement->start);
        // else_expression->as.statement = else_statement;
        // else_expression->end = else_statement->end;
        ast_node_t *else_expression = parse_expression(parser, inside_type_context);
        an_else(expression_node) = else_expression;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static bool is_incoming_function_signature(parser_t* parser) {
    ASSERT(parser->previous.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    lexer_t look_ahead_lexer = parser->lexer;
    i32 parenthesis_level = 1;
    token_t next = parser->current;
    // get matching close parenthesis
    while (true) {
        if (next.type == TOKEN_EOF) {
            error(parser, ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS);
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

static bool is_incoming_declaration_declaration(parser_t* parser) {
    if (parser->current.type != TOKEN_IDENTIFIER) {
        return false;
    }

    lexer_t lookahead_lexer = parser->lexer;

    token_t token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static ast_node_t* definition_declaration(parser_t* parser, bool as_parameter);

static ast_nodes_t parse_parameters(parser_t *parser) {
    ast_nodes_t parameters = {.allocator=&parser->ast->allocator};
    until (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        if (is_incoming_declaration_declaration(parser)) {
            array_push(&parameters, definition_declaration(parser, true));
        } else {
            array_push(&parameters, parse_expression(parser, true));
        }
        if (match(parser, TOKEN_COMMA)) {
            continue;
        }
    }

    return parameters;
}

static void parse_function_signature(parser_t *parser, ast_node_t *function_definition) {
    ASSERT(function_definition->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");
    function_definition->as.function.parameter_nodes = (ast_nodes_t){.allocator=&parser->ast->allocator};
    function_definition->as.function.return_type_expression = NULL;

    function_definition->as.function.parameter_nodes = parse_parameters(parser);

    consume(parser, TOKEN_PARENTHESIS_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS);

    if (match(parser, TOKEN_ARROW_RIGHT)) {
        function_definition->as.function.return_type_expression = parse_expression(parser, true);
    }
}

static ast_node_t *parse_function_definition(parser_t *parser, bool inside_type_context) {
    ast_node_t *function_definition_expression = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, inside_type_context, parser->previous);
    function_definition_expression->as.function.compilable = true;
    function_definition_expression->as.function.block = parse_block(parser, inside_type_context);
    function_definition_expression->end = parser->previous;

    return function_definition_expression;
}

static ast_node_t *parse_grouping_or_function_signature_or_definition(parser_t *parser, bool inside_type_context) {
    ast_node_type_t node_type;
    if (is_incoming_function_signature(parser)) {
        node_type = AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE;
    } else  {
        node_type = AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    ast_node_t* expression_node = ast_node_new(parser->ast, node_type, inside_type_context, parser->previous);

    if (node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        parse_function_signature(parser, expression_node);

        expression_node->value_type = typeid(TYPE_UNRESOLVED);
        expression_node->end = parser->previous;

        if (match(parser, TOKEN_BRACE_OPEN)) {
            ast_node_t *definition = parse_function_definition(parser, inside_type_context);
            expression_node = convert_function_definition(parser, expression_node, definition);
        }
    } else {
        an_operand(expression_node) = parse_expression(parser, inside_type_context);

        consume(parser, TOKEN_PARENTHESIS_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS);

        expression_node->value_type = an_operand(expression_node)->value_type;
    }

    expression_node->end = parser->previous;

    return expression_node;
}

static ast_nodes_t parse_arguments(parser_t* parser, bool inside_type_context) {
    ast_nodes_t arguments = {.allocator = &parser->ast->allocator};

    if (!check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            ast_node_t *argument = parse_expression(parser, inside_type_context);
            array_push(&arguments, argument);
        } while (match(parser, TOKEN_COMMA));
    }

    consume(parser, TOKEN_PARENTHESIS_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS);

    if (arguments.count > MAX_PARAMETERS - 1) {
        error(parser, ERROR_PARSER_TOO_MANY_PARAMETERS);
    }

    return arguments;
}

static ast_node_t *parse_call(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_CALL, inside_type_context, parser->previous);

    expression_node->as.call.callee = NULL;
    expression_node->as.call.arguments = parse_arguments(parser, inside_type_context);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_unary(parser_t *parser, bool inside_type_context) {
    ast_node_t *expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_UNARY, inside_type_context, parser->previous);
    expression_node->operator = parser->previous;

    an_operand(expression_node) = parse_precedence(parser, inside_type_context, PREC_UNARY);
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_binary(parser_t *parser, bool inside_type_context) {
    ast_node_t* expression_node = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_BINARY, inside_type_context, parser->previous);

    token_t operator = parser->previous;
    expression_node->operator = operator;

    parse_rule_t* rule = parser_get_rule(operator.type);

    expression_node->start = parser->previous;

    ast_node_t *rhs = parse_precedence(parser, inside_type_context, (Precedence)(rule->precedence + 1));
    an_rhs(expression_node) = rhs;
    expression_node->end = parser->previous;

    return expression_node;
}

static ast_node_t *parse_struct_def(parser_t *parser, bool inside_type_context) {
    (void)inside_type_context;

    ast_node_t *struct_definition = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION, inside_type_context, parser->previous);

    consume(parser, TOKEN_BRACE_OPEN, ERROR_PARSER_EXPECTED_OPEN_BRACE);

    while (!check(parser, TOKEN_BRACE_CLOSE) && !check(parser, TOKEN_EOF)) {
        ast_node_t *declaration_node = definition_declaration(parser, false);
        array_push(&struct_definition->children, declaration_node);
    }

    consume(parser, TOKEN_BRACE_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_BRACE);

    struct_definition->end = parser->previous;

    return struct_definition;
}

static ast_node_t *parse_dot(parser_t* parser, bool inside_type_context) {
    if (match(parser, TOKEN_BRACE_OPEN)) {
        ast_node_t *initiailizer = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER, inside_type_context, parser->previous);
        if (!match(parser, TOKEN_BRACE_CLOSE)) {
            do {
                if (check(parser, TOKEN_COMMA) || check(parser, TOKEN_BRACE_CLOSE)) {
                    array_push(&initiailizer->as.initiailizer.arguments, NULL);
                } else {
                    ast_node_t *argument = parse_expression(parser, inside_type_context);
                    array_push(&initiailizer->as.initiailizer.arguments, argument);
                }

            } while (match(parser, TOKEN_COMMA));

            consume(parser, TOKEN_BRACE_CLOSE, ERROR_PARSER_EXPECTED_CLOSE_BRACE);
        }

        initiailizer->end = parser->previous;

        return initiailizer;
    } else {
        ast_node_t *dot_expression = ast_node_new(parser->ast, AST_NODE_TYPE_EXPRESSION_DOT, inside_type_context, parser->previous);
        consume(parser, TOKEN_IDENTIFIER, ERROR_PARSER_DOT_OPERATOR_ONLY_NAMES_FOR_NOW);
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
    [TOKEN_EQUAL]                   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_BANG]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_LESS]                    = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER]                 = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_SEMICOLON]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BAR]                     = { NULL,               parse_binary,       PREC_BITWISE_OR },
    [TOKEN_AMPERSAND]               = { parse_unary,        NULL,               PREC_UNARY },
    [TOKEN_PLUS_PLUS]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { NULL,               NULL,               PREC_NONE },
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
    [TOKEN_IF]                      = { parse_ifelse,       NULL,               PREC_BLOCK },
    [TOKEN_THEN]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_UNLESS]                  = { parse_ifelse,       NULL,               PREC_BLOCK },
    [TOKEN_WHILE]                   = { parse_ifelse,       NULL,               PREC_BLOCK },
    [TOKEN_UNTIL]                   = { parse_ifelse,       NULL,               PREC_BLOCK },
    [TOKEN_FOR]                     = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DO]                      = { NULL,               NULL,               PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_TRUE]                    = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_FALSE]                   = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_NULL]                    = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_RETURN]                  = { parse_return,       NULL,               PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_EOF]                     = { NULL,               NULL,               PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,               NULL,               PREC_NONE },
};

static bool check_expression(parser_t *parser) {
    return parser_get_rule(parser->current.type)->prefix != NULL;
}

static ast_node_t *parse_precedence(parser_t *parser, bool inside_type_context, Precedence precedence) {
    advance(parser);

    ParseFn prefix_rule = parser_get_rule(parser->previous.type)->prefix;
    ast_node_t *left_operand;

    if (prefix_rule == NULL) {
        error(parser, ERROR_PARSER_EXPECTED_EXPRESSION);
        left_operand = ast_node_new(parser->ast, AST_NODE_TYPE_NONE, inside_type_context, parser->previous);
    } else {
        left_operand = prefix_rule(parser, inside_type_context);
        left_operand->inside_type_context = inside_type_context;
    }

    while (precedence <= parser_get_rule(parser->current.type)->precedence) {
        advance(parser);
        ParseFn infix_rule = parser_get_rule(parser->previous.type)->infix;
        ast_node_t *right_operand = infix_rule(parser, inside_type_context);
        right_operand->inside_type_context = inside_type_context;

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
        for (size_t i = 0; i < left_operand->as.function.parameter_nodes.count; ++i) {
            ast_node_t *parameter = left_operand->as.function.parameter_nodes.items[i];
            if (!ast_node_type_is_expression(parameter->node_type)) {
                error_at(parser, &parameter->start, ERROR_PARSER_EXPECTED_TYPE);
                break;
            }
        }
    }

    return left_operand;
}

static parse_rule_t *parser_get_rule(token_type_t type) {
    return &rules[type];
}

static ast_node_t *parse_expression(parser_t *parser, bool inside_type_context) {
    bool fold = false;
    if (match(parser, TOKEN_DIRECTIVE)) {
        token_t directive = parser->previous;
        fold = (directive.view.length - 1 == strlen("fold") && strncmp(directive.view.data + 1, "fold", 4) == 0);

        bool is_type_directive = (directive.view.length - 1 == strlen("type") && strncmp(directive.view.data + 1, "type", 4) == 0);
        if (is_type_directive) {
            inside_type_context = is_type_directive;
        }
    }

    ast_node_t* expression_node = parse_precedence(parser, inside_type_context, inside_type_context ? PREC_OR : PREC_ASSIGNMENT);
    expression_node->fold = fold;

    return expression_node;
}

static ast_node_t *parse_statement(parser_t* parser, bool omit_end_of_statement) {
    ast_node_t *statement_node = ast_node_new(parser->ast, AST_NODE_TYPE_DECLARATION_STATEMENT, false, parser->current);
    an_expression(statement_node) = parse_expression(parser, false);
        
    if (!omit_end_of_statement) {
        consume(parser, TOKEN_SEMICOLON, ERROR_PARSER_EXPECTED_SEMICOLON);
    }

    statement_node->end = parser->previous;

    return statement_node;
}

static ast_node_t *definition_declaration(parser_t *parser, bool as_parameter) {
    ast_node_t *definition_node = ast_node_new(parser->ast, AST_NODE_TYPE_DECLARATION_DEFINITION, false, parser->current);

    advance(parser);

    definition_node->value_type = typeid(TYPE_UNRESOLVED);

    definition_node->value_index = value_index_nil();

    definition_node->identifier = parser->previous;

    consume(parser, TOKEN_COLON, ERROR_PARSER_EXPECTED_TYPE);

    if (!check(parser, TOKEN_EQUAL) && !check(parser, TOKEN_COLON)) {
        an_decl_type(definition_node) = parse_expression(parser, true);
    }

    // TODO: try to do constant vs variable detection a little more clever...
    bool requires_expression = false;
    if (an_decl_type(definition_node)->node_type == AST_NODE_TYPE_NONE) {
        if (match(parser, TOKEN_EQUAL)) {
            requires_expression = true;
            definition_node->is_mutable = true;
        } else if (match(parser, TOKEN_COLON)) {
            requires_expression = true;
        }
    } else {
        if (match(parser, TOKEN_EQUAL)) {
            definition_node->is_mutable = true;
        } else {
            consume(parser, TOKEN_COLON, ERROR_PARSER_EXPECTED_DECLARATION_COLON);
        }
        requires_expression = true;
    }

    if (requires_expression) {
        an_decl_expr(definition_node) = parse_expression(parser, false);
    }

    if (an_decl_expr(definition_node)->node_type == AST_NODE_TYPE_NONE) {
        definition_node->is_mutable = true;
    }

    if (!as_parameter) {
        consume(parser, TOKEN_SEMICOLON, ERROR_PARSER_EXPECTED_SEMICOLON);
    }

    definition_node->end = parser->previous;
    return definition_node;
}

static ast_node_t *parse_declaration(parser_t *parser, bool is_top_level) {
    ast_node_t *node = NULL;
    if (is_incoming_declaration_declaration(parser)) {
        node = definition_declaration(parser, false);
    } else {
        unless (is_top_level) {
            node = parse_statement(parser, false);
        } else {
            error_at_current(parser, ERROR_PARSER_EXPECTED_DECLARATION);
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

    ast->root = parse_precedence(&parser, false, PREC_BLOCK);

    consume(&parser, TOKEN_EOF, ERROR_PARSER_EXPECTED_EOF);

    return !parser.had_error;
}

bool parse(ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn) {
    parser_t parser = {0};
    parser_init(&parser, ast, file_path, source, error_fn);

    advance(&parser);

    ast->root = ast_node_new(parser.ast, AST_NODE_TYPE_EXPRESSION_BLOCK, false, parser.previous);

    while (!match(&parser, TOKEN_EOF)) {
        ast_node_t *declaration_node = parse_declaration(&parser, true);
        if (declaration_node) {
            array_push(&ast->root->children, declaration_node);
        }
    }

    consume(&parser, TOKEN_EOF, ERROR_PARSER_EXPECTED_EOF);

    return !parser.had_error;
}

value_index_t add_value_to_ast_constant_stack(ast_t *ast, void *data, type_t type) {
    type_info_t *type_info = get_type_info(&ast->type_set.types, type);
    size_t size = type_info->size;

    size_t index;
    if (memarr_push(&ast->constants, data, size, &index)) {
        return value_index_(index);
    }

    return value_index_nil();
}

value_index_t zero_value(ast_t *ast, type_t type) {
    size_t result = 0;
    if (table_get(ptr2sizet, ast->type_to_zero_index, type, &result)) {
        return value_index_(result);
    }

    type_info_t *type_info = ast->type_set.types.items[type.i];

    word_t value[bytes_to_slots(type_info->size)];

    switch (type_info->kind) {
        case TYPE_POINTER:
        case TYPE_VOID:
        case TYPE_BOOL:
        
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
            for (size_t i = 0; i < bytes_to_slots(type_info->size); i++) {
                value[i] = WORDI(0);
            }
            break;
        }

        case TYPE_COUNT:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_TYPE:
        case TYPE_INVALID:
        case TYPE_UNDEFINED:
        case TYPE_UNRESOLVED:
            UNREACHABLE();
            value[0] = WORDI(0);
            break;
    }

    value_index_t zero_index = add_value_to_ast_constant_stack(ast, value, type);
    if (zero_index.exists) {
        table_put(ptr2sizet, ast->type_to_zero_index, type, zero_index.index);
    }

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
            print_line("nil");
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
            cstr_t branch = NULL;
            unless (node->looping) {
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

            if (an_is_notnone(an_else(node))) {
                print_indent(level + 1);
                print_line("else");
                ast_print_ast_node(types, an_else(node), level + 2);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_CALL: {
            print_indent(level);
            print_line("call: %s", type2cstr(node));

            print_indent(level+1);
            print_line("callee");
            ast_print_ast_node(types, node->as.call.callee, level + 2);
            
            print_indent(level+1);
            print_line("arguments");

            for (size_t i = 0; i < node->as.call.arguments.count; ++i) {
                ast_node_t *arg = node->as.call.arguments.items[i];
                ast_print_ast_node(types, arg, level+2);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            print_indent(level);
            print_line("function definition: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("definition");
            ast_print_ast_node(types, node->as.function.block, level + 2);
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

        case AST_NODE_TYPE_EXPRESSION_RETURN: {
            print_indent(level);
            print_line("return");

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
            print_line("<undefined>");
            break;
        }
    }

    allocator_return(tmp);
}

void ast_print(ast_t* ast, const char* name) {
    printf("=== %s ===\n", name);
    ast_print_ast_node(ast->type_set.types, ast->root, 0);
}
