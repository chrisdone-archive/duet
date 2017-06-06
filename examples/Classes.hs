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
data Maybe a = Nothing | Just a

class F (a :: Type -> Type) where
  fm :: a Nat

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

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
notEqual = \x y -> not (equal x y)
demo = equal (reader (shower (Succ Zero))) (Succ Zero)
