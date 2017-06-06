{-# LANGUAGE ExplicitForAll #-}
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
