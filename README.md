# The Orso Programming Language
Orso is a small, portable, statically-typed, expression-based language with powerful metaprogramming and compile-time evaluation tools.

It maintains a simple and terse syntax making it easy to learn. The terseness of the syntax is intended to rival Lua's.

## Key Features
- Statically typed 
- Expression-based
- Arbitrary Expression Compile-Time Evaluation
- First-Class Types
- Manual Memory Mangement
- Easy C interop

## Syntax
Orso is expression-based which means almost all of its language constructs evaluate to some "value". In fact, the only language construct that is not an expression is
a variable declaration or compile-time value declaration. As a result, this makes the language quite flexible in regards to where expressions can be placed in the code. As such,
for now, semicolons are required to signal the end of an expression.

### Variable and Compile-Time Value Declarations
These are the only non-expressions constructs in the entire language, although they technically evaluate to `void`. However, from a pragmatic sense, the syntax makes
it impossible to ever use these constructs anywhere that needs an expression to be evaluated.

```python
# variable declarations
x := 0;
y: sint = 0;

# compile-time value declarations
PI :: 3.14;
TAU : double : PI*2;
```

### Expressions
Some of these features are not implemeneted yet, and I've left comments pointing those out.

#### Block Expressions
```python
temperature := {
  wind_chill := 4;
  air_tmp := 1;
  wind_chill + air_tmp; # block evaluates to final statement
};
```

#### If Expressions
``` python
is_hot_outside := if temperature > 25 then "yes" else "no";
```

#### Do-Block Expressions
```python
# allows for a break to return an expression
is_cold_outside := do {
  if is_hot_outside then break "no";
  if temperature < 0 then "yes" else "cool";
};
```

#### Loops and Labeled Do-Blocks
```python
hotter_day := do:find for month := 0; month < 12; ++month {
  for day := 0; day < 30; ++day {
     temp := get_temperature(month, day);
     if temp < temperature then break:find temp; # breaks out of do block with label
  };
} then 0; 

```

#### Unreachable Expressions
```python
input := {
  # input is an "unreachable" type if there's no input
  unless has_input() then return;
  get_input();
};

# `&` is the operator to take an address of an l-value
handle_input(&input);
```


#### Function Definition Expressions
```python
add :: (a: sint, b: sint) -> void {
  return a + b;
};

# short-hand (not implemented yet)
add :: (a: sint, b: sint) -> void => a + b;
```

#### Struct Definition Expressions
```python
# (not implemented yet)
bar_t :: struct {
  drink_count: sint;
};
```

#### Type Expressions
Types are also treated like expressions.
```python
alias_for_sint :: sint;

var_for_sint := int;

# compare them like regular values
alias_for_sint != var_for_sint and var_for_sint == int;
```

There are many more expressions, but these examples should highlight the idea.

### Metaprogramming
While the metaprogramming features are minimal, they are quite powerful.

#### Inferred Function Definition
```python
add :: (a: !u, b: u) -> u {
  return a + b;
};

# copy of add is created for `double`s, inferred by the `1.5` argument
add(1.5, 2.6);

# copy of add is created for `sint`s, inferred by the `5` argument
add(5, 10);
```

#### Inferred Struct Definitions
todo

#### Arbitrary Expression Compile-Time Evaluation

This can be used to evaluate values you want to be present at compile-time.

```python
gen_pi :: (precision: double) -> double {
  pi := # evaluate PI using an interative approach
  return pi;
};

# `@run` directive attempts to run any expression at compile-time
PI :: @run gen_pi(0.000000001);
```

Since everything is an expression, you can also do this inside the type annotation
```python
get_real_type :: () -> type => if HIGH_PRECISION then double else float;
real : @run get_real_type() = 10 .0; 
```

This will run the `get_real_type` at compile-time and provide a compile-time type value to the type annotation to use to define the variable.

#### Macros
todo

#### Custom Compiler Directives
todo






