project('orso', 'c',
  version : '0.1',
  default_options : ['warning_level=3', 'c_std=c99'])

add_global_arguments(['-Wunused-function'], language: 'c')

inc = include_directories('include')

src = [
  'src/chunk.c',
  'src/codegen.c',
  'src/debug.c',
  'src/interpreter.c',
  'src/lexer.c',
  'src/parser.c',
  'src/static_analyzer.c',
  'src/symbol_table.c',
  'src/virtual_machine.c',
]

# test_variable_retrieval = executable('test_variable_retrieval', [src, 'test/variable_retrieval.c'],
#   install : false,
#   include_directories : inc)

c_args = []
if get_option('debug_trace')
  c_args += '-DDEBUG_TRACE_EXECUTION'
endif

if get_option('debug_print')
  c_args += '-DDEBUG_PRINT'
endif

executable('orso', [src, 'main.c'], install : false, include_directories : inc, c_args : c_args)

# test('variable_retrieval', test_variable_retrieval)
