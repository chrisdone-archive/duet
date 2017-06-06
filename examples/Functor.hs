class Functor (functor :: Type -> Type) where
  map :: forall before after. (before -> after) -> functor before -> functor after
