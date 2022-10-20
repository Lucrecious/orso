#include "savine.h"

#include "abstract_syntax_tree.h"
#include "chunk.h"
#include "lexer.h"

static void contextualize(Savine_Context* context, Chunk* chunk) { }

void savine_init(Savine_Context* context) {

}

void savine_run_code(Savine_Context* context, char* code) {
    LexerState state;
    lexer_state_init(&state, code);

    AbstractSyntaxTree ast;
    abstract_syntax_tree_init(&ast);
    
    savine_parse_tree(&state, &ast);

    print_tree(&ast);

    abstract_syntax_tree_free(&ast);
}

void savine_get_i64(Savine_Context* context, char* idenifier, long* value) {
}
