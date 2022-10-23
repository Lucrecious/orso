#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "abstract_syntax_tree.h"
#include "common.h"

typedef struct OrsoStaticAnalyzer {
    OrsoErrorFunction error_fn;
    bool had_error;
    bool panic_mode;
} OrsoStaticAnalyzer;

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn);

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, OrsoExpressionNode* expression);

void orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast);


#endif