# Haskell In Haskell

This is my attempt at writing an understandable implementation of a subset
of Haskell, in Haskell itself.

## Usage

This project can be built with `cabal build`, or installed
with `cabal install`. Installation should put an executable named
`haskell-in-haskell` on your path. Otherwise, you can run the project
directly with `cabal run haskell-in-haskell --`, followed by the arguments
you'd like to pass.

### Compiling

To compile a Haskell file, just run:

```
haskell-in-haskell compile in.hs out.c
```

`in.hs` is the Haskell input file, and `out.c` is the C code to generate.

The compiler only works with single Haskell files. To run the generated code,
you'll need to have a C compiler handy, and to make sure that
the `runtime.c` file is in the same directory, before running:

```
gcc -std=c99 out.c
```

(Replacing `gcc` by your C compiler of choice)

The C generated should conform to the C99 standard. You can also
pass `-DDEBUG` to C compiler, which will enable some debug printing.
This will print out some Garbage Collection information, and a "stack trace"
of program execution:

```
hs__entry
indirection_entry
hs_main
indirection_entry
hs_foo
hs__S_add
hs_foo__S__S_2
hs_increment
hs_foo__S__S_2__S__S_3
hs_increment_case_0
GC Done. 0x00040 ↓ 0x00040 ↑ 0x000C0
hs__S_add
hs_foo_x
hs__S_add_case_0
hs_increment_case_0__S__S_0
hs__S_add_case_0_case_0
hs__S_add_case_0
hs_foo_x
hs__S_add_case_0_case_0
hs__entry_case_0
```

### Stages

You can also see the compiler's output after various stages, so:


This will print the tokens after running the Lexer stage

```
haskell-in-haskell lex in.hs
```

This will print the parsed AST:

```
haskell-in-haskell parse in.hs
```

This will print the simplified AST:

```
haskell-in-haskell simplify in.hs
```

This will print the AST after type-checking:

```
haskell-in-haskell type in.hs
```

This will print the "STG" for your code, which is an intermediate
functional IR:

```
haskell-in-haskell stg in.hs
```

Finally, this will print the "CMM", which is the last stage before
generating C code:

```
haskell-in-haskell cmm in.hs
```

# Features

The first big missing thing is `IO`. The way `main` works in this version
of Haskell is that it must either be `main :: Int`, or `main :: String`,
and the program evaluates `main` before printing out its value.

In terms of features we have:

Your standard top level definitions and bindings:

```haskell
x :: Int
x = 33

y :: String
y = 4

z :: Int -> Int
z 3 = 0
z _ = 4
```

We have let expressions:

```haskell
x =
  let z = 3
      y = 4
  in z + y
```

As well as where expressions:

``` haskell
x = z + y
  where
    z = 3
    y = 4
```

Your standard arithmetic operators:

```haskell
x = 1 + 3 / 45 * 6 - 34
```

Strings, and concatenation:

```haskell
x = "foo" ++ "bar"
```

Booleans:

``` haskell
true :: Bool
true = True

false :: Bool
false = False

z :: Bool
z = 3 > 4
```

If expressions:

``` haskell
x = if 1 > 2 then 3 else 5
```

Case expressions:

```haskell
case z of
  1 -> 3
  x -> x
  _ -> 4
  
case x of
  "foo" -> "bar"
  _ -> "baz"
```

This extends to your standard multiple function heads:

``` haskell
add 0 0 = 0
add n 0 = n
add n m = add n (m - 1)
```

We have polymorphic functions:

``` haskell
id :: a -> a
id x = x
```

As well as your standard polymorphic ADTs:

``` haskell
data List a = Cons a (List a) | Nil

length :: List a -> Int
length Nil = 0
length (Cons _ rest) = 1 + length rest
```

Finally, we have type synonyms, which can't be polymorphic:

```haskell
type ListInt = List X

type X = Int
```

Everything beyond that has not been implemented. I'd say this is a
respectable chunk of Haskell 98, but some glaring omissions include
`IO`, modules, and typeclasses, as well as the entire standard library.

## Implementation Features

In terms of the implementation, this is based of the 1992 `STG` paper,
so we have lazy evaluation, garbage collection, and updates, to avoid
evaluating closures twice.

## Examples

The `/examples` directory is a bit poor, but the `/integration_tests`
directory should have a few more examples of things accepted by the compiler.

# Contributing

If you want to help fix a bug, a first recommendation is to use
the following compilation options for your C output:

```
gcc -DDEBUG -g -fsanitize=address  -fsanitize=undefined -std=c99 .output.c                                                              
```

This will enable `asan` and `ubsan`, which provide runtime checks
against common errors with C. This is immensely useful in catching
bad usage of memory as early as possible.

This is the source of most bugs in the compiler.

# Resources

I'm currently writing [a series](https://cronokirby.com/series/haskell-in-haskell/)
going through the implementation of this compiler in depth, so I'd recommend
looking through that if you want to understand more.

https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf
https://homepages.inf.ed.ac.uk/wadler/papers/pattern/pattern.pdf
http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
