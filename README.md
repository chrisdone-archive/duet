# <img src=images/duet.svg height=36> Duet

A tiny language, a subset of Haskell (with type classes) aimed at aiding teachers teach Haskell

## Run

Running code in Duet literally performs one substitution step at
time. For example, evaluating `(\x -> x + 5) (2 * 3)`, we get:

``` haskell
$ duet run demo.hs
(\x -> x + 5) (2 * 3)
(2 * 3) + 5
6 + 5
11
```

Note that this demonstrates basic argument application and non-strictness.

## Docker run

Run with the docker distribution, to easily run on any platform:

    $ docker run -it -v`pwd`:`pwd` -w`pwd` chrisdone/duet run foo.hs

The image is about 11MB, so it's quick to download.

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
* Duet does not have `seq`, but it does have bang patterns in
  cases. `case x of !x -> ..` is a perfectly legitimate way to force a
  value.
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

## I/O

Basic terminal input/output is supported.

For example,

    $ duet run examples/terminal.hs --hide-steps
    Please enter your name:
    Chris
    Hello, Chris

And with steps:

    $ duet run examples/terminal.hs
    PutStrLn "Please enter your name: " (GetLine (\line -> PutStrLn (append "Hello, " line) (Pure 0)))
    Please enter your name:
    GetLine (\line -> PutStrLn (append "Hello, " line) (Pure 0))
    Chris
    (\line -> PutStrLn (append "Hello, " line) (Pure 0)) "Chris"
    PutStrLn (append "Hello, " "Chris") (Pure 0)
    Hello, Chris
    Pure 0

How does this work? Whenever the following code is seen in the
stepper:

```haskell
PutStrLn "Please enter your name: " <next>
```

The string is printed to stdout with `putStrLn`, and the `next`
expression is stepped next.

Whenever the following code is seen:

``` haskell
GetLine (\line -> <next>)
```

The stepper runs `getLine` and feeds the resulting string into the
stepper as:

```haskell
(\line -> <next>) "The line"
```

This enables one to write an example program like this:

``` haskell
data Terminal a
 = GetLine (String -> Terminal a)
 | PutStrLn String (Terminal a)
 | Pure a

main =
  PutStrLn
    "Please enter your name: "
    (GetLine (\line -> PutStrLn (append "Hello, " line) (Pure 0)))
```
