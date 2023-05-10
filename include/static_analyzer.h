#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"
#include "type_set.h"

typedef struct OrsoDeclaredState {
    OrsoSymbolTable scope;
    struct OrsoDeclaredState* outer;
} OrsoDeclaredState;

typedef struct OrsoStaticAnalyzer {
    OrsoFunctionDeclarationNode* function;

    OrsoErrorFunction error_fn;
    OrsoSymbolTable symbol_to_type;
    OrsoTypeSet type_set;

    OrsoSymbolTable symbols;

    bool had_error;
    bool panic_mode;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn);
void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer);

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast);


#endif
