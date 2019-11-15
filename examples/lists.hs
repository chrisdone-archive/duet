data List a = Nil | Cons a (List a)
map = \f xs ->
  case xs of
    Nil -> Nil
    Cons x xs -> Cons (f x) (map f xs)
list = (Cons 1 (Cons 2 Nil))
multiply = \x y -> x * y
doubleAll = \xs -> map (multiply 2) xs
main = doubleAll list
