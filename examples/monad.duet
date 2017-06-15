class Monad (m :: Type -> Type) where
  bind :: m a -> (a -> m b) -> m b
class Applicative (f :: Type -> Type) where
  pure :: a -> f a
  ap :: f (a -> b) -> f a -> f b
class Functor (f :: Type -> Type) where
  map :: (a -> b) -> f a -> f b
data Maybe a = Nothing | Just a
instance Functor Maybe where
  map =
    \f m ->
      case m of
        Nothing -> Nothing
        Just a -> Just (f a)
instance Monad Maybe where
  bind =
    \m f ->
      case m of
        Nothing -> Nothing
        Just v -> f v
instance Applicative Maybe where
  pure = \v -> Just v
  ap = \a b -> Nothing
