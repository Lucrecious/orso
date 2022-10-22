project('savine', 'c',
  version : '0.1',
  default_options : ['warning_level=3', 'c_std=c99'])

add_global_arguments(['-Wunused-function'], language: 'c')

inc = include_directories('include')

src = [
  'src/abstract_syntax_tree.c',
  'src/chunk.c',
  'src/codegen.c',
  'src/debug.c',
  'src/lexer.c',
  'src/static_analyzer.c',
  'src/str_to_value.c',
  'src/value.c',
  'src/virtual_machine.c',
]

# test_variable_retrieval = executable('test_variable_retrieval', [src, 'test/variable_retrieval.c'],
#   install : false,
#   include_directories : inc)

executable('savine', [src, 'main.c'], install : false, include_directories : inc)

# test('variable_retrieval', test_variable_retrieval)
