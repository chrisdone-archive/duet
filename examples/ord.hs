class Ord a  where
  compare :: a -> a -> Ordering
data Ordering
  = EQ
  | LT
  | GT
instance Ord Ordering where
  compare =
    \x y ->
      case x of
        LT ->
          case y of
            LT -> EQ
            EQ -> LT
            GT -> LT
        EQ ->
          case y of
            LT -> GT
            EQ -> EQ
            GT -> LT
        GT ->
          case y of
            LT -> GT
            EQ -> GT
            GT -> EQ
main = compare EQ LT
