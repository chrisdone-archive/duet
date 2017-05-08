# Duet

A programming language focused on interactive collaboration between
the developer and the computer.

## Web releases

* [Alpha](http://chrisdone.com/toys/duet-alpha/)
* [Beta](http://chrisdone.com/toys/duet-beta/)
* [Gamma](http://chrisdone.com/toys/duet-gamma/)

## Command-line usage

Run with the file and function to evaluate:

    $ stack exec duet File.hs main

Output looks like:

``` haskell
$ stack exec duet examples/X.hs main
-- Type checking ...
Just :: forall g0. g0 -> Maybe g0
Nothing :: forall g0. Maybe g0
Left :: forall g0 g1. g0 -> Either g0 g1
Right :: forall g0 g1. g1 -> Either g0 g1
X :: Either (Maybe Bool) Bool -> X
Y :: Y
-- Source:
compose = (\f g x -> ((f :: g4 -> g5) (((g :: g3 -> g4) (x :: g3) :: g4)) :: g5) :: (g4 -> g5) -> (g3 -> g4) -> g3 -> g5)
id = (\x -> (x :: g7) :: g7 -> g7)
and = (\x y -> (if (x :: Bool) then (if (y :: Bool) then (True :: Bool) else (False :: Bool) :: Bool) else (False :: Bool) :: Bool) :: Bool -> Bool -> Bool)
main = ((Just :: String -> Maybe String) ((if (True :: Bool) then ("ok!" :: String) else ("nope" :: String) :: String)) :: Maybe String)
-- Stepping ...
Just (if True then "ok!" else "nope")
Just "ok!"
```

## Looks like

The below is a pretty comprehensive example of supported syntax so
far:

``` haskell
data List a = Nil | Cons a (List a)
data Tuple a b = Tuple a b
id = \x -> x
not = \p -> if p then False else True
foldr = \cons nil l ->
  case l of
    Nil -> nil
    Cons x xs -> cons x (foldr cons nil xs)
foldl = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl f (f z x) xs
map = \f xs ->
  case xs of
    Nil -> Nil
    Cons x xs -> Cons (f x) (map f xs)
zip = \xs ys ->
  case Tuple xs ys of
    Tuple Nil _ -> Nil
    Tuple _ Nil -> Nil
    Tuple (Cons x xs1) (Cons y ys1) ->
      Cons (Tuple x y) (zip xs1 ys1)
list = (Cons True (Cons False Nil))
main = zip list (map not list)
```

## Holes

Anything prefixed with `_` is a hole of any type. The substitutor does
not try to expand it. This is useful for seeing how code evaluates for
any `f` or writing proofs:

```haskell
data List a = Nil | Cons a (List a)
foldr = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)
foldl = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl f (f z x) xs
list = (Cons True (Cons False Nil))
```

 ```haskell
> main = foldr _f _nil list
foldr _f _nil list
_f True (foldr _f _nil (Cons False Nil))
_f True (_f False (foldr _f _nil Nil))
_f True (_f False _nil)
```

 ```haskell
> main = foldl _f _nil list
foldl _f _nil list
foldl _f (_f _nil True) (Cons False Nil)
foldl _f (_f (_f _nil True) False) Nil
_f (_f _nil True) False
```

## Type-classes

What remains to implement is a resolver of instances. An expression
like `show 'a'` currently has type `Show Char => String` in my
AST. Now I have to insert instance dictionaries as arguments (or
annotations) to expressions, and then type-classes will be good to go.
