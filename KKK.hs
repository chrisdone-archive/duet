data List a = Nil | Cons a (List a)
foldr0 = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> f x (foldr0 f z xs)
foldl = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl f (f z x) xs
list = (Cons True (Cons False Nil))
main = foldl (\xs x -> Cons x xs) Nil list
