# Duet

A programming language focused on interactive collaboration between
the developer and the computer.

## Web releases

* [Alpha](http://chrisdone.com/toys/duet-alpha/)
* [Beta](http://chrisdone.com/toys/duet-beta/)
* [Gamma](http://chrisdone.com/toys/duet-gamma/)

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
