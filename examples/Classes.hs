class Read a where
  read :: List Char -> a
class Show a where
  show :: a -> String
instance Show Nat where
  show = \n ->
    case n of
      Zero -> Cons Z Nil
      Succ n -> Cons S (show n)
data Nat = Succ Nat | Zero
instance Read Nat where
  read = \cs ->
    case cs of
      Cons Z Nil -> Zero
      Cons S xs  -> Succ (read xs)
      _ -> Zero
data List a = Nil | Cons a (List a)

data Char = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

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
main = equal (read (show (Succ Zero))) (Succ Zero)
