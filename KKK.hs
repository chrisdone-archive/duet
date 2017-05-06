data List a = Nil | Cons a (List a)
foldr0 = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> f x (foldr0 f z xs)
foldl0 = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl0 f (f z x) xs
main = foldl (\xs x -> Cons x xs) Nil list
