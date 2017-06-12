data Unit = Unit
class Monad (m :: Type -> Type) where
  bind :: m a -> (a -> m b) -> m b
class Applicative (f :: Type -> Type) where
  pure :: a -> f a
class Functor (f :: Type -> Type) where
  map :: (a -> b) -> f a -> f b
data Result s a = Result s a
data State s a = State (s -> Result s a)
instance Functor (State s) where
  map =
    \f state ->
      case state of
        State s2r ->
          State
            (\s ->
               case s2r s of
                 Result s1 a -> Result s1 (f a))
instance Monad (State s) where
  bind =
    \m f ->
      case m of
        State s2r ->
          State
            (\s ->
               case s2r s of
                 Result s a ->
                   case f a of
                     State s2r1 -> s2r1 s)
instance Applicative (State s) where
  pure = \a -> State (\s -> Result s a)
runState =
  \m a ->
    case m of
      State f -> f a
get = State (\s -> Result s s)
put = \s -> State (\k -> Result s Unit)
next = \m n -> bind m (\_ -> n)
main = runState (next (put False) (pure Unit)) True
