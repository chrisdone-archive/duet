data List a = Nil | Cons a (List a)
foldr = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> f x (foldr f z xs)
foldl = \f z l ->
  case l of
    Nil -> z
    Cons x xs -> foldl f (f z x) xs
foldl_ = \f z l ->
  case l of
    Nil -> z
    Cons x xs ->
      case f z x of
        !z_ -> foldl_ f z_ xs
list = (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
main_foldr = foldr (\x y -> x + y) 0 list
main_foldl = foldl (\x y -> x + y) 0 list
main_foldl_ = foldl_ (\x y -> x + y) 0 list
