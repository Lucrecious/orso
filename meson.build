project('savine', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

inc = include_directories('include')

src = ['src/savine.c', 'src/lexer.c', 'src/array.c', 'src/list.c', 'src/abstract_syntax_tree.c', 'src/savine_string.c', 'src/stringtable.c']

test_array = executable('test_array', [src, 'test/array.c'],
  install : false,
  include_directories : inc)

test_list = executable('test_list', [src, 'test/list.c'],
  install : false,
  include_directories : inc)

test_assignment = executable('test_assignment', [src, 'test/assignment.c'],
  install : false,
  include_directories : inc)

executable('savine', [src, 'main.c'], install : false, include_directories : inc)

test('array', test_array)
test('list', test_list)
test('assignment', test_assignment)
