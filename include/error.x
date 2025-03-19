// Disable IntelliSense for this file
// c_cpp_properties.json

// note i can optionally give a command that gives you more information on error

X(ERROR_NONE, "none", PARSER)

X(ERROR_PARSER_EXPECTED_JMP_LABEL_AFTER_COLON, "expected a jump label", PARSER)
X(ERROR_PARSER_EXPECTED_CLOSE_BRACE, "expected close brace for block", PARSER)
X(ERROR_PARSER_EXPECTED_DO_OR_BRACE_AFTER_LOOP_CONDITION, "expected 'do' or '{' after while/until condition", PARSER)
X(ERROR_PARSER_EXPECTED_THEN_OR_BRACE_AFTER_BRANCH_CONDITION, "expected 'then' or '{' after if/until condition", PARSER)
X(ERROR_PARSER_EXPECTED_EXPRESSION, "expected expression", PARSER)
X(ERROR_PARSER_EXPECTED_CLOSE_PARENTHESIS, "expected ')' for the matching '('", PARSER)
X(ERROR_PARSER_EXPECTED_CLOSE_BRACKET_AFTER_SIZE_EXPRESSION, "expected ']' for the matching '['", PARSER)
X(ERROR_PARSER_EXPECTED_OPEN_BRACE_AFTER_STRUCT, "expected '{' after 'struct' to specify its members", PARSER)
X(ERROR_PARSER_EXPECTED_OPEN_PARENTHESIS_BUILTIN_FUNC, "expected '(' after builtin function keyword", PARSER)
X(ERROR_PARSER_EXPECTED_VALID_TOKEN_AFTER_DOT, "expected an identifier (member access), '{' (struct initializer), or '[' (array initializer) after a '.'", PARSER)
X(ERROR_PARSER_EXPECTED_COLON_AFTER_DECLARATION_IDENTIFIER, "expected ':' after declaration identifier", PARSER)
X(ERROR_PARSER_EXPECTED_EOF_AFTER_EXPRESSION, "expected the end of the file after the expression", PARSER)
X(ERROR_PARSER_INVALID_DECLARATION_DIRECTIVE, "invalid declaration directive", PARSER)
X(ERROR_PARSER_EXPECTED_IDENTIFER_AFTER_INFERRED_TYPE_DECL_ANNOTATION, "expected identifer after an inferred type declaration annotation", PARSER)
X(ERROR_PARSER_EXPECTED_SEMICOLON, "expected ';'", PARSER)

X(WARNING_PARSER_NUMBER_OVERFLOW, "expected ';'", PARSER)

X(ERROR_PARSEREX_EXPECTED_EOF_AFTER_MODULE, "expected the end of the file after the module", PARSEREX)
X(ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_STRUCT_DECLARATION, "expected ';' after a declaration inside a struct block", PARSEREX)
X(ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_DECLARATION, "expected ';' after a statement or definition", PARSEREX)
X(ERROR_PARSEREX_EXPECTED_TYPE, "expected type", PARSEREX)
X(ERROR_PARSEREX_EXPECTED_DECLARATION, "expected declaration", PARSEREX)
X(ERROR_PARSEREX_EXPECTED_EXPRESSION, "expected expression", PARSEREX)
X(ERROR_PARSEREX_TOO_MANY_PARAMETERS, "too many parameters", PARSEREX)

X(ERROR_ANALYSIS_INVALID_UNARY_OPERAND, "invalid unary operand", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_TAKE_ADDRESS_OF_CONSTANT, "cannot take address of constant", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_OPERAND_FOR_ADDRESS_OPERATOR, "invalid operand for address operator", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES, "type is not capable of arithmetics", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES, "type is not capable of comparisons", ANALYSIS)
X(ERROR_ANALYSIS_NUMBER_MUST_BE_ON_RHS_FOR_POINTER_ARITHMETIC, "number must be on right hand side for pointer arithmetic", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES, "type is not capable of logical operations", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_EXPRESSION_FOR_LIST_INIT, "invalid expression for list initializer", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_ACCESSOR_TYPE, "accessor type must be unsigned or signed int", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_ACCESSEE_TYPE, "accessee type must be an array or pointer to an array", ANALYSIS)
X(ERROR_ANALYSIS_LHS_REQUIRES_TYPE_KNOWN_AT_COMPILE_TIME, "left hand side must be a type known at compile time", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_CAST, "invalid cast operand; cannot perform cast", ANALYSIS)
X(ERROR_ANALYSIS_ARITHMETIC_OPERANDS_REQUIRES_EXPLICIT_CAST, "arithmetic operands requires explicit cast", ANALYSIS)
X(ERROR_ANALYSIS_ONLY_ADD_AND_SUB_ARE_VALID_IN_PTR_ARITHMEIC, "invalid pointer arithmetic; only addition and subtraction are allowed", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_ADD_POINTERS, "cannot add pointers together", ANALYSIS)
X(ERROR_ANALYSIS_INITIALIZER_LIST_TYPE_CANNOT_BE_INFERRED, "initializer list type cannot be inferred", ANALYSIS)
X(ERROR_ANALYSIS_COMPARISON_OPERANDS_REQUIRES_EXPLICIT_CAST, "comparison operands requires explicit cast", ANALYSIS)
X(ERROR_ANALYSIS_LOGICAL_OPERANDS_REQUIRES_EXPLICIT_CAST, "logical operands requires explicit cast", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_NOT_OPERAND, "invalid 'not' operand", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_DEREF_OPERAND, "only pointers can be dereferenced", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_DEREFERENCE_VOIDPTR, "cannot deference a void pointer", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_MINUS_OPERAND, "invalid '-' operand", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_NEGATE_UNSIGNED_NUMBER, "cannot negate unsigned number", ANALYSIS)
X(ERROR_ANALYSIS_NUMBER_ARGS_CALL_FUNC_MISTMATCH, "number of arguments for call do not match function type's parameter count", ANALYSIS)
X(ERROR_ANALYSIS_ARG_VS_PARAM_FUNC_CALL_MISMATCH, "argument type does not match function parameter type", ANALYSIS)
X(ERROR_ANALYSIS_TYPEOF_REQUIRES_ONE_ARG, "'typeof' builtin function requires exactly one argument", ANALYSIS)
X(ERROR_ANALYSIS_SIZEOF_REQUIRES_ONE_ARG, "'sizeof' builtin function requires exactly one argument", ANALYSIS)
X(ERROR_ANALYSIS_LEN_REQUIRES_ONE_ARRAY_ARG, "'len' builtin function requires exactly one array or pointer to array argument", ANALYSIS)
X(ERROR_ANALYSIS_SIZEOF_BUILTIN_REQUIRES_A_CONSTANT_TYPE_OR_ANOTHER_EXPRESSION_TYPE, "'sizeof' builtin requires a constant type or another expression type", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_BINARY_OPERANDS, "invalid binary operands", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_MEMBER_ACCESS, "invalid member access", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_RETURN_TYPE, "invalid return type", ANALYSIS)
X(ERROR_ANALYSIS_EXPECTED_CONSTANT, "expected compile-time constant", ANALYSIS)
X(ERROR_ANALYSIS_TOO_MANY_ARGUMENTS_IN_LIST_INIT, "too many arguments in list initializer", ANALYSIS)
X(ERROR_ANALYSIS_TYPE_MISMATCH, "type mismatch", ANALYSIS)
X(ERROR_ANALYSIS_EXPECTED_LVALUE, "expected lvalue", ANALYSIS)
X(ERROR_ANALYSIS_EXPECTED_CALLABLE, "expected callable", ANALYSIS)
X(ERROR_ANALYSIS_INFERRED_CALLEE_MUST_BE_CONSTANT, "inferred function calls must be resolved at compile-time", ANALYSIS)
X(ERROR_ANALYSIS_EXPECTED_RESOLVED, "expected resolved", ANALYSIS)
X(ERROR_ANALYSIS_EXPECTED_TYPE, "expected type", ANALYSIS)
X(ERROR_ANALYSIS_INFERRED_TYPE_DECLS_ARE_ONLY_ALLOWED_FOR_FUNCTIONDEFS, "inferred type decls are only allowed inside function headers", ANALYSIS)
X(ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_A_SIGNED_OR_UNSIGNED_INTEGER, "array size must be a signed or unsigned integer", ANALYSIS)
X(ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_A_COMPILE_TIME_CONSTANT, "array size must a compile-time constant", ANALYSIS)
X(ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_POSITIVE, "array size must positive", ANALYSIS)
X(ERROR_ANALYSIS_ARRAY_TYPE_MUST_BE_COMPILE_TIME_CONSTANT, "array type must a compile-time constant", ANALYSIS)
X(ERROR_ANALYSIS_ARRAY_TYPE_IS_NOT_A_TYPE, "array type is not a type", ANALYSIS)
X(ERROR_ANALYSIS_COULD_NOT_PATTERN_MATCH_TYPE, "could not pattern match type; todo: better errors for this", ANALYSIS)
X(ERROR_ANALYSIS_JMP_RETURN_TYPE_DOES_NOT_MATCH_BLOCKS, "jmp's return type does not match block's", ANALYSIS)
X(ERROR_ANALYSIS_ASSIGNEE_TYPE_DOES_NOT_MATCH_DEF_TYPE, "assignee type does not match definition's type", ANALYSIS)
X(ERROR_ANALYSIS_ARG_TYPE_DOES_NOT_MATCH_PARAM_TYPE, "argument type does not match parameter's type", ANALYSIS)
X(ERROR_ANALYSIS_INITIAL_EXPR_TYPE_MISMATCH, "initial expression type does not match the declaration's type", ANALYSIS)
X(ERROR_ANALYSIS_INTRINSIC_DECLARATIONS_CAN_ONLY_BE_TYPES, "intrinsic definitions can only be type values", ANALYSIS)
X(ERROR_ANALYSIS_INTRINSIC_NAME_DOES_NOT_EXIST, "intrinsic name does not exist", ANALYSIS)
X(ERROR_ANALYSIS_INFERRED_FUNCDEF_CANNOT_BE_SET_TO_MUTABLE_VAR, "inferred function definition can only be assigned to compile-time variables", ANALYSIS)
X(ERROR_ANALYSIS_ONLY_INTRINSIC_FUNCTIONS_ARE_SUPPORTED, "only intrinsic functions are supported", ANALYSIS)
X(ERROR_ANALYSIS_INTRINSIC_MUST_BE_CONSTANT, "intrinsic definitions can only be type values", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_RETURN_INSIDE_RETURN, "cannot return inside return", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_ACCESS_MUTABLE_ON_DIFFERENT_FOLD_LEVEL, "cannot access mutable on different fold level", ANALYSIS)
X(ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST, "definition does not exist", ANALYSIS)
X(ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST_IN_THE_SAME_RUN_SCOPE, "definition does not exist in the same run scope as where it's being evaluated", ANALYSIS)
X(ERROR_ANALYSIS_CAN_ONLY_ACCESS_CONSTANTS_AND_GLOBALS, "only function locals, constants in higher scopes, and globals are accessible from within a function definition", ANALYSIS)
X(ERROR_ANALYSIS_FUNCTION_PART_OF_CYCLICAL_DEPENDENCY, "function is defined circularly", ANALYSIS)
X(ERROR_ANALYSIS_TYPE_INFERENCE_DECL_HAS_NOT_BEEN_RESOLVED, "type has not been inferred", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION, "cannot overload definition", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_FOLD_ON_ERRORED_FUNCTION_DEFINITION, "cannot fold on errored function definition", ANALYSIS)
X(ERROR_ANALYSIS_FOLDING_LOOP, "folding loop", ANALYSIS)
X(ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES, "function must return on all branches", ANALYSIS)
X(ERROR_ANALYSIS_MEMBER_DOES_NOT_EXIST, "member does not exist", ANALYSIS)
X(ERROR_ANALYSIS_INVALID_TYPE_FOR_TYPE_INITIALIZER_LIST, "invalid type for type initializer", ANALYSIS)
X(ERROR_ANALYSIS_TOO_MANY_STRUCT_ARGUMENTS, "too many struct arguments", ANALYSIS)
X(ERROR_ANALYSIS_BLOCKS_MUST_BE_EMPTY_OR_END_IN_STATEMENT, "blocks must be empty or end in a statement", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_STORE_VOID_EXPRESSIONS, "cannot store void expressions", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_INFER_NIL_VALUE, "cannot infer nil value", ANALYSIS)
X(ERROR_ANALYSIS_BLOCKS_TYPE_MISMATCH, "block types do not match", ANALYSIS)
X(ERROR_ANALYSIS_CONDITION_MUST_BE_BOOL, "condition must be bool", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_JMP_IN_CONDITION, "cannot jmp in condition", ANALYSIS)
X(ERROR_ANALYSIS_NO_VALID_JMP_BLOCK, "invalid scope for jmp", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_FIND_JMP_LABEL, "invalid scope for jmp", ANALYSIS)
X(ERROR_ANALYSIS_CANNOT_RETURN_OUTSIDE_FUNC_DEF, "cannot return outside of a function definition", ANALYSIS)

X(ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG, "memory size too big", CODEGEN)
X(ERROR_CODEGEN_JMP_TOO_LARGE, "jump too big", CODEGEN)
X(ERROR_CODEGEN_STACK_SIZE_GROWS_LARGER_THAN_UINT32_MAX, "stack size cannot grow larger than (2^32)-1", CODEGEN)
X(ERROR_CODEGEN_DATA_TOO_LARGE_TO_PUSH_TO_STACK, "stack size cannot grow larger than (2^32)-1", CODEGEN)
X(ERROR_CODEGEN_OFFSET_TOO_LARGE, "instruction mov offset too large", CODEGEN)