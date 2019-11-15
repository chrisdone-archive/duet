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
  in Duet (this is intentional).
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
