data List a = Nil | Cons a (List a)
foldr = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)
foldl = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl f (f z x) xs
list = (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
main_foldr = foldr (+) _nil list
main_foldl = foldl (+) _nil list
