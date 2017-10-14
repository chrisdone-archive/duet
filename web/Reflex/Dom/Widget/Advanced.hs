{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Reflex.Dom.Widget.Advanced where

import           Control.Lens.Basic
import           Data.Functor.Identity
import           Reflex.Dom.Class
import           Reflex.Dom.Widget.Basic
-- import Reflex.Dom.Internal.Foreign ()

import           Prelude hiding (mapM, mapM_, sequence, sequence_)
import           Reflex
import           Reflex.Host.Class
import           Data.Functor.Misc
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Dependent.Sum (DSum (..))
import           Data.Foldable
import           Data.Traversable
import           Control.Monad.Trans
import           Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import           Control.Monad.State hiding (state, mapM, mapM_, forM, forM_, sequence, sequence_)
import           GHCJS.DOM.Node
import           GHCJS.DOM.UIEvent
import           GHCJS.DOM.EventM (on, event, EventM, stopPropagation)
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element as E
import           GHCJS.DOM.Types hiding (Event)
import qualified GHCJS.DOM.Types as DOM (Event)
import           GHCJS.DOM.NamedNodeMap as NNM
import           Data.These
import           Data.Align
import           Data.Maybe
import           Data.Bitraversable
import           GHCJS.DOM.MouseEvent
import           Data.IORef
import           Data.Default

{-

1. Modify these to support getting more detailed event information. So
instead of EventType and EventResult, we write our own getters.

E.g.

getKeyEvent :: EventM e KeyboardEvent Int
getKeyEvent = do
  e <- event
  which <- getWhich e
  if which /= 0 then return which else do
    charCode <- getCharCode e
    if charCode /= 0 then return charCode else
      getKeyCode e

2. Possibly support propagation prevention (with a pure predicate?).

-}

-- data PressedKey = PressedKey
--   { pressedKeyWhich :: Int
--   }

-- type family EventResultTypeAdvanced (en :: EventTag) :: * where
--   EventResultTypeAdvanced 'KeypressTag = KeypressEvent

wrapDomEventsMaybe_
  :: ( Functor (Event t)
     , IsElement e
     , MonadIO m
     , MonadSample t m
     , MonadReflexCreateTrigger t m
     , Reflex t
     , HasPostGui t h m
     )
  => e
  -> (forall en. EventName en -> EventM e (EventType en) (Maybe (f en)))
  -> m (EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe_ element handlers = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  e <- newFanEventWithTrigger $ \(WrapArg en) et -> do
        unsubscribe <- onEventName en element $ do
          mv <- handlers en
          forM_ mv $ \v -> liftIO $ postGui $ runWithActions [et :=> Identity v]
        return $ liftIO $ do
          unsubscribe
  return $! e

wrapElement_
  :: forall t h m.
     ( Functor (Event t)
     , MonadIO m
     , MonadSample t m
     , MonadReflexCreateTrigger t m
     , Reflex t
     , HasPostGui t h m
     )
  => (forall en. Element -> EventName en -> EventM Element (EventType en) (Maybe (EventResult en)))
  -> Element
  -> m (El t)
wrapElement_ eh e = do
  es <- wrapDomEventsMaybe_ e $ eh e
  return $ El e es

{-# INLINABLE elWith'_ #-}
elWith'_
  :: (MonadWidget t m, Attributes m attrs)
  => String -> ElConfig attrs -> m a -> m (El t, a)
elWith'_ elementTag cfg child = do
  (e, result) <- buildElementNS (view namespace cfg) elementTag (view attributes cfg) child
  e' <- wrapElement_ defaultDomEventHandler e
  return (e', result)

{-# INLINABLE el'_ #-}
el'_
  :: forall t m a.
     MonadWidget t m
  => String -> m a -> m (El t, a)
el'_ elementTag = elWith'_ elementTag def
