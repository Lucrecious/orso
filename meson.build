project('savine', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

add_global_arguments(['-Wunused-function', '-std=c99'], language: 'c')

inc = include_directories('include')

src = [
  'src/savine.c',
  'src/lexer.c',
  'src/abstract_syntax_tree.c',
  'src/savine_string.c',
  'src/stringtable.c',
]

test_assignment = executable('test_assignment', [src, 'test/assignment.c'],
  install : false,
  include_directories : inc)

executable('savine', [src, 'main.c'], install : false, include_directories : inc)

test('assignment', test_assignment)
