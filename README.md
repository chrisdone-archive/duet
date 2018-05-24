# Duet

A programming language focused on interactive collaboration between
the developer and the computer.

## Status

This project is a work in progress and an experiment; do not open
issues or pull requests, they will be ignored.

## Web releases

* [Epsilon](http://chrisdone.com/toys/duet-epsilon/) - December 18 2017
* [Delta](http://chrisdone.com/toys/duet-delta/) - June 19 2017
* [Gamma](http://chrisdone.com/toys/duet-gamma/) - May 2nd 2017
* [Beta](http://chrisdone.com/toys/duet-beta/) - April 30th 2017
* [Alpha](http://chrisdone.com/toys/duet-alpha/) - April 27th 2017

## Building locally

    $ stack build

The web UI can be built with `sh build.sh`, but probably nobody but me
needs that.

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
class Reader a where
  reader :: List Ch -> a
class Shower a where
  shower :: a -> List Ch
instance Shower Nat where
  shower = \n ->
    case n of
      Zero -> Cons Z Nil
      Succ n -> Cons S (shower n)
data Nat = Succ Nat | Zero
instance Reader Nat where
  reader = \cs ->
    case cs of
      Cons Z Nil -> Zero
      Cons S xs  -> Succ (reader xs)
      _ -> Zero
data List a = Nil | Cons a (List a)
data Ch = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
class Equal a where
  equal :: a -> a -> Bool
instance Equal Nat where
  equal =
    \a b ->
      case a of
        Zero ->
          case b of
            Zero -> True
            _ -> False
        Succ n ->
          case b of
            Succ m -> equal n m
            _ -> False
        _ -> False
not = \b -> case b of
              True -> False
              False -> True
notEqual :: Equal a => a -> a -> Bool
notEqual = \x y -> not (equal x y)
main = equal (reader (shower (Succ Zero))) (Succ Zero)
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

Type-classes are supported, as in this example:

``` haskell
data Maybe a = Nothing | Just a
class Functor (f :: Type -> Type) where
  map :: forall a b. (a -> b) -> f a -> f b
instance Functor Maybe where
  map = \f m ->
    case m of
      Nothing -> Nothing
      Just a -> Just (f a)
not = \b -> case b of
              True -> False
              False -> True
main = map not (Just True)
```

Kind inference is not implemented, so if you want a kind other than
`Type` (aka `*` in Haskell), you have to put a kind signature on the
type variable.
