#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"

typedef struct OrsoDefinedVariables {
    OrsoSymbolTable scope;
    struct OrsoDefinedVariables* outer;
} OrsoDefinedVariables;

typedef struct OrsoStaticAnalyzer {
    OrsoErrorFunction error_fn;
    OrsoSymbolTable symbol_to_type;

    OrsoSymbolTable symbols;
    //OrsoSymbolTable defined_variables;

    OrsoDefinedVariables defined_variables_scopes;
    OrsoDefinedVariables* defined_variables_bottom_scope;

    bool had_error;
    bool panic_mode;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn);
void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer);

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast);


#endif
