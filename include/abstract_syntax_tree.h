#ifndef ABSTRACT_SYNTAX_TREE_H_
#define ABSTRACT_SYNTAX_TREE_H_

#include <stdlib.h>

#include "error_codes.h"
#include "lexer.h"

typedef enum NodeType {
    NodeType_INVALID,
    NodeType_PROGRAM,
    NodeType_STATEMENT_LIST,
    NodeType_STATEMENT,
    NodeType_ASSIGNMENT,
    NodeType_EXPRESSION,
    NodeType_IDENTIFIER,
    NodeType_INTEGER,
} NodeType;

typedef struct IdentifierNode {
    char* value;
} IdentifierNode;

typedef struct IntegerNode {
    i32 value;
} IntegerNode;

typedef struct ExpressionNode {
    NodeType type;
    union Node {
        IdentifierNode identifier;
        IntegerNode integer;
    } node;
} ExpressionNode;

typedef struct AssignmentNode {
    IdentifierNode identifier;
    ExpressionNode* expression;
} AssignmentNode;

typedef struct StatementNode {
    AssignmentNode* assignment;
} StatementNode;

typedef struct ProgramNode {
    StatementNode** statement_ptr_list;
} ProgramNode;

typedef struct AbstractSyntaxTree {
    ProgramNode* program;
} AbstractSyntaxTree;

typedef struct TreeParserState {
    AbstractSyntaxTree* tree;
    LexerState* lexer_state;
    Token consumed;
    Token consuming;
    SavineError error;
} TreeParserState;

void abstract_syntax_tree_init(AbstractSyntaxTree* ast);
void abstract_syntax_tree_free(AbstractSyntaxTree* ast);

void savine_parse_tree(LexerState* lexer_state, AbstractSyntaxTree* tree);

void print_tree(AbstractSyntaxTree* ast);

#endif
