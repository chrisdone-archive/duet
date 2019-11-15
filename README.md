# Duet

A tiny language, a subset of Haskell aimed at aiding teachers teach Haskell

## Usage

This comes as a library and as a program capable of running Duet programs.

```
$ duet --help
Duet interpreter

Usage: duet [--version] [--help] COMMAND
  This is the interpreter for the Duet mini-Haskell educational language

Available options:
  --version                Show version
  --help                   Show this help text

Available commands:
  run                      Run the given program source
```

## Run

The help for this command:

```
Usage: duet run FILEPATH [--main NAME] [--concise] [-n|--steps steps]
  Run the given program source

Available options:
  -h,--help                Show this help text
  FILEPATH                 The .hs file to interpret
  --main NAME              The main value to run
  --concise                Concise view
  -n,--steps steps         Maximum number of steps to run (default: 100)
```

Running code in Duet literally performs one substitution step at
time. For example, evaluating `(\x -> x + 5) (2 * 3)`, we get:

``` haskell
[1]
(\x -> x + 5) (2 * 3)
[2]
(2 * 3) + 5
[3]
6 + 5
[4]
11
```

Note that this demonstrates basic argument application and non-strictness.

Example `integers.hs`:

```haskell
main = 3 + ((2 + -3) - 3)
```

Output for this program:

``` haskell
$ duet run examples/integers.hs
[1]
3 + ((2 + -3) - 3)
[2]
3 + (-1 - 3)
[3]
3 + -4
[4]
-1
```

## Differences from Haskell

See also the next section for a complete example using all the
available syntax.

* Duet is non-strict, but is not lazy. There is no sharing and no
  thunks.
* No `module` or `import` module system whatsoever.
* No `let` syntax, no parameters in definitions e.g. `f x = ..` you
  must use a lambda. Representing `let` in the stepper presents a
  design challenge not currently met.
* Kinds `*` are written `Type`: e.g. `class Functor (f :: Type -> Type)`.
* Kind inference is not implemented, so if you want a kind other than
  `Type` (aka `*` in Haskell), you have to put a kind signature on the
  type variable.
* Indentation is stricter, a case's alts must be at a column larger
  than the `case`.
* Infix operators are stricter: an infix operator must have spaces
  around it. You **cannot** have more than one operator without
  parentheses, therefore operator precedence does not come into play
  in Duet (this is intentional). This also permits you to write `-5`
  without worrying about where it rests.
* Superclasses are not supported.
* Operator definitions are not supported.
* There is only `Integer` and `Rational` number types: they are
  written as `1` or `1.0`.
* Any `_` or `_foo` means "hole" and the interpreter does not touch
  them, it continues performing rewrites without caring. This is good
  for teaching.
* There is no standard `Prelude`. The only defined base types are:
  * String
  * Char
  * Integer
  * Rational
  * Bool
* You don't need a `Show` instance to inspect values; the interpreter
  shows them as they are, including lambdas.

View `examples/syntax-buffet.hs` for an example featuring all the
syntax supported in Duet.
