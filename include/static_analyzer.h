#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "abstract_syntax_tree.h"
#include "common.h"

void orso_resolve_expression(OrsoExpressionNode* expression);

void orso_resolve_ast_types(OrsoAST* ast, OrsoErrorFunction error_fn);


#endif