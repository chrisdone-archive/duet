class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
data List a = Nil | Cons a (List a)
instance Monoid (List a) where
  mempty = Nil
  mappend = \x y ->
    case x of
      Cons a xs -> Cons a (mappend xs y)
      Nil -> y
main = mappend (Cons 'a' (Cons 'b' Nil)) (Cons 'c' (Cons 'd' Nil))
