// note i can optionally give a command that gives you more information on error

ERROR_XMACRO(ERROR_NONE, "none", PARSER)

ERROR_XMACRO(ERROR_PARSER_EXPECTED_JMP_LABEL, "expected a jump label", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_CLOSE_BRACE_FOR_BLOCK, "expected close brace for block", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_DO_OR_BRACE_AFTER_LOOP_CONDITION, "expected 'do' or '{' after while/until condition", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_THEN_OR_BRACE_AFTER_BRANCH_CONDITION, "expected 'then' or '{' after if/until condition", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS, "expected ')' for the matching '('", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_OPEN_BRACE_AFTER_STRUCT, "expected '{' after 'struct' to specify its members", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_OPEN_PARENTHESIS_BUILTIN_FUNC, "expected '(' after builtin function keyword", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_CLOSE_BRACE_AFTER_STRUCT_DECLARATIONS, "expected '}' after then struct's declarations block", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_CLOSE_BRACE_AFTER_STRUCT_INITIALIZER_ARGUMENTS, "expected '}' after arguments within a struct initializer", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_VALID_TOKEN_AFTER_DOT, "expected an identifier (member access), '{' (struct initializer), or '[' (array initializer) after a '.'", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_COLON_AFTER_DECLARATION_IDENTIFIER, "expected ':' after declaration identifier", PARSER)
ERROR_XMACRO(ERROR_PARSER_EXPECTED_EOF_AFTER_EXPRESSION, "expected the end of the file after the expression", PARSER)

ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_EOF_AFTER_MODULE, "expected the end of the file after the module", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_STRUCT_DECLARATION, "expected ';' after a declaration inside a struct block", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_DECLARATION, "expected ';' after a statement or definition", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_TYPE, "expected type", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_DECLARATION, "expected declaration", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_EXPECTED_EXPRESSION, "expected expression", PARSEREX)
ERROR_XMACRO(ERROR_PARSEREX_TOO_MANY_PARAMETERS, "too many parameters", PARSEREX)

ERROR_XMACRO(ERROR_ANALYSIS_INVALID_UNARY_OPERAND, "invalid unary operand", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES, "type is not capable of arithmetics", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES, "type is not capable of comparisons", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES, "type is not capable of logical operations", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_ARITHMETIC_OPERANDS_REQUIRES_EXPLICIT_CAST, "arithmetic operands requires explicit cast", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_COMPARISON_OPERANDS_REQUIRES_EXPLICIT_CAST, "comparison operands requires explicit cast", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_LOGICAL_OPERANDS_REQUIRES_EXPLICIT_CAST, "logical operands requires explicit cast", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_NOT_OPERAND, "invalid 'not' operand", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_MINUS_OPERAND, "invalid '-' operand", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_NEGATE_UNSIGNED_NUMBER, "cannot negate unsigned number", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_NUMBER_ARGS_CALL_FUNC_MISTMATCH, "number of arguments for call do not match function type's parameter count", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_ARG_VS_PARAM_FUNC_CALL_MISMATCH, "argument type does not match function parameter type", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_TYPEOF_REQUIRES_ONE_ARG, "'typeof' builtin function requires exactly one argument", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_BINARY_OPERANDS, "invalid binary operands", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_MEMBER_ACCESS, "invalid member access", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_RETURN_TYPE, "invalid return type", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_EXPECTED_CONSTANT, "expected compile time constant", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_EXPECTED_LVALUE, "expected lvalue", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_EXPECTED_CALLABLE, "expected callable", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_EXPECTED_RESOLVED, "expected resolved", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_EXPECTED_TYPE, "expected type", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_TYPE_MISMATCH, "type mismatch", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_RETURN_INSIDE_RETURN, "cannot return inside return", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_ACCESS_MUTABLE_ON_DIFFERENT_FOLD_LEVEL, "cannot access mutable on different fold level", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION, "cannot overload definition", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_FOLD_ON_ERRORED_FUNCTION_DEFINITION, "cannot fold on errored function definition", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_FOLDING_LOOP, "folding loop", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES, "function must return on all branches", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_MEMBER_DOES_NOT_EXIST, "member does not exist", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_INVALID_TYPE_FOR_TYPE_INITIALIZER_LIST, "invalid type for type initializer", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_TOO_MANY_STRUCT_ARGUMENTS, "too many struct arguments", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_BLOCKS_MUST_BE_EMPTY_OR_END_IN_STATEMENT, "blocks must be empty or end in a statement", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_STORE_VOID_EXPRESSIONS, "cannot store void expressions", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_BLOCK_RETURNS_MISMATCH, "block returns do not match", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CONDITION_MUST_BE_BOOL, "condition must be bool", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_JMP_IN_CONDITION, "cannot jmp in condition", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_NO_VALID_JMP_BLOCK, "invalid scope for jmp", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_FIND_JMP_LABEL, "invalid scope for jmp", ANALYSIS)
ERROR_XMACRO(ERROR_ANALYSIS_CANNOT_RETURN_OUTSIDE_FUNC_DEF, "cannot return outside of a function definition", ANALYSIS)

ERROR_XMACRO(ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG, "memory size too big", CODEGEN)
ERROR_XMACRO(ERROR_CODEGEN_JMP_TOO_LARGE, "jump too big", CODEGEN)
ERROR_XMACRO(ERROR_CODEGEN_STACK_SIZE_GROWS_LARGER_THAN_UINT32_MAX, "stack size cannot grow larger than (2^32)-1", CODEGEN)