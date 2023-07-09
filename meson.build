project('orso', 'c',
  version : '0.1',
  default_options : ['warning_level=3', 'c_std=c99', '-Db_sanitize=address'])

add_global_arguments(['-Wunused-function'], language: 'c')

inc = include_directories('include')

src = [
  'src/chunk.c',
  'src/codegen.c',
  'src/debug.c',
  'src/interpreter.c',
  'src/lexer.c',
  'src/object.c',
  'src/parser.c',
  'src/slot.c',
  'src/static_analyzer.c',
  'src/symbol_table.c',
  'src/type.c',
  'src/type_set.c',
  'src/virtual_machine.c',
]

c_args = ['-DDEBUG']
if get_option('debug_trace')
  c_args += '-DDEBUG_TRACE_EXECUTION'
endif

if get_option('debug_print')
  c_args += '-DDEBUG_PRINT'
endif

executable('debug-orso', [src, 'main.c'], install : false, include_directories : inc, c_args : c_args)

executable('orso', [src, 'main.c'], install : false, include_directories: inc, c_args : ['-O3'])

test_c_args = ['-DDEBUG_GC_STRESS', '-DDEBUG']

test_error = executable('test_error', [src, 'test/error.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_variable_declaration = executable('test_variable_declaration', [src, 'test/variable_declaration.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_assignment = executable('test_assignment', [src, 'test/assignment.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_arithmetic = executable('test_arithmetic', [src, 'test/arithmetic.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_control_flow = executable('test_control_flow', [src, 'test/control_flow.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_logical_operators = executable('test_logical_operators', [src, 'test/logical_operators.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_loops = executable('test_loops', [src, 'test/loops.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)

test_file_runner = executable('test_file_runner', [src, 'test/test_file_runner.c'],
  install : false,
  include_directories : inc,
  c_args : test_c_args)


#configure_file('./test/function_tests.orso', join_paths(meson.current_build_dir(), 'function_tests.orso'), copy: true)

test('test_error', test_error)
test('test_variable_declaration', test_variable_declaration)
test('test_assignment', test_assignment)
test('test_arithmetic', test_arithmetic)
test('test_control_flow', test_control_flow)
test('test_logical_operators', test_logical_operators)
test('test_loops', test_loops)

test('test_functions', test_file_runner, args: files('test/functions.orso'))
test('test_compile_time_evaluation', test_file_runner, args: files('test/compile_time_evaluation.orso'))


