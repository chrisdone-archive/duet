data List a = Nil | Cons a (List a)
not = \p -> if p
               then False
               else True
map = \f l ->
  case l of
    Nil -> Nil
    Cons a xs -> Cons (f a) (map f xs)
main = map not (Cons True (Cons False Nil))
