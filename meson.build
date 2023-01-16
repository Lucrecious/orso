project('orso', 'c',
  version : '0.1',
  default_options : ['warning_level=3', 'c_std=c99'])

add_global_arguments(['-Wunused-function'], language: 'c')

inc = include_directories('include')

src = [
  'src/chunk.c',
  'src/codegen.c',
  'src/debug.c',
  'src/garbage_collector.c',
  'src/interpreter.c',
  'src/lexer.c',
  'src/object.c',
  'src/parser.c',
  'src/static_analyzer.c',
  'src/symbol_table.c',
  'src/virtual_machine.c',
]

test_error = executable('test_error', [src, 'test/error.c'],
  install : false,
  include_directories : inc)

test_variable_declaration = executable('test_variable_declaration', [src, 'test/variable_declaration.c'],
  install : false,
  include_directories : inc)

test_assignment = executable('test_assignment', [src, 'test/assignment.c'],
  install : false,
  include_directories : inc)

c_args = []
if get_option('debug_trace')
  c_args += '-DDEBUG_TRACE_EXECUTION'
endif

if get_option('debug_print')
  c_args += '-DDEBUG_PRINT'
endif

if get_option('debug_gc_stress')
  c_args += '-DDEBUG_GC_STRESS'
endif

if get_option('debug_gc_print')
  c_args += '-DDEBUG_GC_PRINT'
endif

executable('orso', [src, 'main.c'], install : false, include_directories : inc, c_args : c_args)

test('test_error', test_error)
test('test_variable_declaration', test_variable_declaration)
test('test_assignment', test_assignment)
