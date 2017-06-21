data List a = Nil | Cons a (List a)
data Tuple a b = Tuple a b
id = \x -> x
not = \p -> if p then False else True
foldr = \cons nil l ->
  case l of
    Nil -> nil
    Cons x xs -> cons x (foldr cons nil xs)
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
list = (Cons 1 (Cons 2 Nil))
multiply = \x y -> x * y
doubleAll = \xs -> map (multiply 2) xs
main = doubleAll list
