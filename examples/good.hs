class Good a where
  good :: a -> Bool
data Maybe a = Just a | Nothing
instance Good Bool where
  good = \x -> x
instance Good a => Good (Maybe a) where
  good = \x ->
    case x of
      Nothing -> False
      Just a -> good a
main = good (Just True)
