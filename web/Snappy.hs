{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | A reactive Snap SVG interface.

module Snappy where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.IORef
import           Data.Maybe
import qualified Data.Text as T
import qualified Snap
import           System.IO.Unsafe

--------------------------------------------------------------------------------
-- Event type

data Event a = forall origin s. Event
  { eventSubscribers :: IORef [origin -> IO ()]
  , eventFromOrigin :: s -> origin -> (Maybe a,s)
  , eventState :: s
  }

instance Functor Event where
  fmap f (Event subscribers fromOrigin state) =
    Event
      subscribers
      (\s origin ->
         let (a', s') = fromOrigin s origin
         in (fmap f a', s'))
      state
  {-# INLINE fmap #-}

-- | Zip events that occur in tandem within the given time frame.
zipEvents :: (a -> b -> c) -> Event a -> Event b -> Event c
zipEvents f e1 e2 =
  unsafePerformIO
    (do subscribersRef <- newIORef mempty
        tvar1 <- newEmptyMVar
        tvar2 <- newEmptyMVar
        let !ev = Event subscribersRef (\s (a, b) -> (Just (f a b), s)) ()
        let listen e us them pair =
              bindEvent
                e
                (\a -> do
                   result <- tryTakeMVar them
                   case result of
                     Nothing -> putMVar us a
                     Just b -> do
                       subscribers <- readIORef subscribersRef
                       mapM_ ($ (pair a b)) subscribers)
        listen e1 tvar1 tvar2 (\a b -> (a, b))
        listen e2 tvar2 tvar1 (\a b -> (b, a))
        pure ev)
{-# NOINLINE zipEvents #-}

scanEvent :: (s -> a -> s) -> s -> Event a -> Event s
scanEvent f nil (Event subscribers fromOrigin oldState) =
  Event
    subscribers
    (\s origin ->
       let (a, _) = fromOrigin oldState origin
           s' = fmap (f s) a
       in (s', fromMaybe s s'))
    nil
{-# INLINE scanEvent #-}

mapMaybeEvent :: (a -> Maybe b) -> Event a -> Event b
mapMaybeEvent f (Event subscribers fromOrigin oldState) =
  Event
    subscribers
    (\s origin ->
       let (a, s') = fromOrigin s origin
       in (a >>= f, s'))
    oldState
{-# INLINE mapMaybeEvent #-}

filterEvent :: (a -> Bool) -> Event a -> Event a
filterEvent p =
  mapMaybeEvent
    (\a ->
       if p a
         then Just a
         else Nothing)

bindEvent :: Event a -> (a -> IO ()) -> IO ()
bindEvent Event {..} m = do
  do stateRef <- newIORef eventState
     modifyIORef
       eventSubscribers
       (++ [ \v -> do
               s <- readIORef stateRef
               let (v', s') = eventFromOrigin s v
               case v' of
                 Nothing -> return ()
                 Just v'' -> m v''
               writeIORef stateRef s'
           ])

--------------------------------------------------------------------------------
-- Dynamic type

data Dynamic a = Dynamic
  { dynDefault :: a
  , dynEvent :: Maybe (Event a)
  }

instance Functor Dynamic where
  fmap f (Dynamic def event) =
    Dynamic (f def) (fmap (fmap f) event)
  {-# INLINE fmap #-}

instance Applicative Dynamic where
  pure a = Dynamic {dynDefault = a, dynEvent = Nothing}
  {-# INLINE pure #-}
  f <*> a =
    Dynamic
    { dynDefault = dynDefault f (dynDefault a)
    , dynEvent =
        case (dynEvent f, dynEvent a) of
          (Nothing, Nothing) -> Nothing
          (Just fevent, Nothing) -> Just (fmap ($ dynDefault a) fevent)
          (Nothing, Just aevent) -> Just (fmap (dynDefault f) aevent)
          (Just fevent, Just aevent) -> Just (zipEvents ($) fevent aevent)
    }
  {-# INLINE (<*>) #-}

zipDynamics
  :: (a -> b -> c)
  -> Dynamic a
  -> Dynamic b
  -> Dynamic c
zipDynamics f d1 d2 =
  Dynamic
  { dynDefault = f (dynDefault d1) (dynDefault d2)
  , dynEvent =
      case (dynEvent d1, dynEvent d2) of
        (Nothing, Nothing) -> Nothing
        (Just d1event, Nothing) -> Just (fmap (\a -> f a (dynDefault d2)) d1event)
        (Nothing, Just d2event) -> Just (fmap (\a -> f (dynDefault d1) a) d2event)
        (Just d1event, Just d2event) -> Just (zipEvents f d1event d2event)
  }

scanDynamic :: (s -> a -> s) -> s -> Event a -> Dynamic s
scanDynamic f nil e =
  Dynamic {dynDefault = nil
          ,dynEvent =  Just (scanEvent f nil e)}

bindDynamic :: Dynamic a -> (a -> IO ()) -> IO ()
bindDynamic (Dynamic _ event) m =
  maybe
    (return ())
    (\e ->
       void
         (forkIO
            (do yield
                bindEvent e m)))
    event

dynamicDef :: Dynamic a -> a
dynamicDef (Dynamic def _) = def

eventToDynamic :: a -> Event a -> Dynamic a
eventToDynamic d e = Dynamic {dynDefault = d, dynEvent = Just e}

--------------------------------------------------------------------------------
-- Drag event

data DragEvent = DragStart Pos | Dragging Drag | DragStop

data Pos = Pos
  { posX :: Double
  , posY :: Double
  }

data Drag = Drag
  { dragDX :: Double
  , dragDY :: Double
  }

data Rec = Rec
  { recX :: Double
  , recY :: Double
  , recW :: Double
  , recH :: Double
  }

dragEvent :: Snap.HasDrag d => d -> IO (Event DragEvent)
dragEvent d = do
  subscribersRef <- newIORef mempty
  Snap.drag
    d
    (\dx dy -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> do subscriber (Dragging (Drag dx dy))) subscribers)
    (\x y -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber (DragStart (Pos x y))) subscribers)
    (\_ -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber DragStop) subscribers)
  st <- newIORef ()
  pure
    (Event
     { eventSubscribers = subscribersRef
     , eventFromOrigin = \s origin -> (Just origin, s)
     , eventState = st
     })

changeEvent :: Snap.Textbox -> IO (Event String)
changeEvent d = do
  subscribersRef <- newIORef mempty
  Snap.change
    d
    (\text -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber text) subscribers)
  threadRef <- newIORef Nothing
  Snap.keyup
    d
    (\k text -> do
       mthreadId <- readIORef threadRef
       maybe (return ()) killThread mthreadId
       threadId <-
         forkIO
           (do threadDelay (1000 * 50)
               subscribers <- readIORef subscribersRef
               mapM_ (\subscriber -> subscriber (T.unpack text)) subscribers)
       atomicWriteIORef threadRef (Just threadId))
  st <- newIORef ()
  pure
    (Event
     { eventSubscribers = subscribersRef
     , eventFromOrigin = \s origin -> (Just origin, s)
     , eventState = st
     })

-- | A dynamic which makes a draggable thing move around upon drag at
-- the given axis.
draggable
  :: Circle
  -> Double
  -> (Drag -> Double)
  -> Dynamic Double
draggable c initial get =
  fmap
    snd
    (scanDynamic
       (\(origin, new) event ->
          case event of
            Dragging pos -> (origin, origin + get pos)
            _ -> (new, new))
       (initial, initial)
       (circleDrag c))

--------------------------------------------------------------------------------
-- Click event

data ClickEvent = ClickEvent
  { clickX :: !Double
  , clickY :: !Double
  , clickModifier :: !Snap.Modifier
  } deriving (Show)

clickEvent :: Snap.HasClick d => d -> IO (Event ClickEvent)
clickEvent d = do
  subscribersRef <- newIORef mempty
  Snap.singleClick
    d
    (\_event modifier x y -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber (ClickEvent x y modifier)) subscribers)
  st <- newIORef ()
  pure
    (Event
     { eventSubscribers = subscribersRef
     , eventFromOrigin = \s origin -> (Just origin, s)
     , eventState = st
     })

--------------------------------------------------------------------------------
-- Circle object

data Circle = Circle
  { circleObject :: Snap.Circle
  , circleDrag :: Event DragEvent
  , circleX :: Dynamic Double
  , circleY :: Dynamic Double
  }

circle :: Snap.Snap -> Dynamic Double -> Dynamic Double -> Dynamic Double -> IO Circle
circle snap xdynamic ydynamic rdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
  c <- Snap.circle snap x y (dynamicDef rdynamic)
  drag <- dragEvent c
  t <- Snap.newMatrix
  xLast <- newIORef 0
  bindDynamic
    xdynamic
    (\x' -> do
       undo <- readIORef xLast
       Snap.translate t (-undo + (x' - x)) 0
       Snap.transform c t
       writeIORef xLast (x' - x))
  yLast <- newIORef 0
  bindDynamic
    ydynamic
    (\y' -> do
       undo <- readIORef yLast
       Snap.translate t 0 (-undo + (y' - y))
       Snap.transform c t
       writeIORef yLast (y' - y))
  pure (Circle c drag xdynamic ydynamic)

--------------------------------------------------------------------------------
-- Rect object

data Rect = Rect
  { rectObject :: Snap.Rect
  , rectDrag :: Event DragEvent
  }

rect
  :: Snap.Snap
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic String
  -> IO Rect
rect snap xdynamic ydynamic wdynamic hdynamic fdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
  c <- Snap.rect snap x y (dynamicDef wdynamic) (dynamicDef hdynamic) 0 0
  Snap.setAttr c "fill" (dynamicDef fdynamic)
  drag <- dragEvent c
  bindDynamic xdynamic (\x' -> Snap.setAttr c "x" x')
  bindDynamic ydynamic (\y' -> Snap.setAttr c "y" y')
  bindDynamic wdynamic (\w' -> Snap.setAttr c "width" w')
  bindDynamic hdynamic (\h' -> Snap.setAttr c "height" h')
  bindDynamic fdynamic (\f' -> Snap.setAttr c "fill" f')
  pure (Rect c drag)

--------------------------------------------------------------------------------
-- Text object

data Text = Text
  { textObject :: Snap.Text
  , textClicked :: Event ClickEvent
  }

text :: Snap.Snap -> Dynamic Double -> Dynamic Double -> Dynamic String -> IO Text
text snap xdynamic ydynamic tdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
  c <- Snap.text snap x y (dynamicDef tdynamic)
  t <- Snap.newMatrix
  xLast <- newIORef 0
  bindDynamic
    xdynamic
    (\x' -> do
       undo <- readIORef xLast
       Snap.translate t (-undo + (x' - x)) 0
       Snap.transform c t
       writeIORef xLast (x' - x))
  yLast <- newIORef 0
  bindDynamic
    ydynamic
    (\y' -> do
       undo <- readIORef yLast
       Snap.translate t 0 (-undo + (y' - y))
       Snap.transform c t
       writeIORef yLast (y' - y))
  bindDynamic tdynamic (\t' -> Snap.setAttr c "#text" t')
  clickev <- clickEvent c
  pure (Text c clickev)

--------------------------------------------------------------------------------
-- Textbox object

data Textbox = Textbox
  { textboxObject :: Snap.Textbox
  , textboxChange :: Event String
  }

textbox
  :: Snap.Snap
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic Double
  -> Dynamic String
  -> IO Textbox
textbox snap xdynamic ydynamic wdynamic hdynamic tdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
      w = dynamicDef wdynamic
      h = dynamicDef hdynamic
  c <- Snap.textbox snap x y w h (dynamicDef tdynamic)
  t <- Snap.newMatrix
  bindDynamic
    xdynamic
    (\x' -> Snap.setAttrInt c "left" x')
  bindDynamic
    ydynamic
    (\x' -> Snap.setAttrInt c "top" x')
  bindDynamic
    wdynamic
    (\x' -> Snap.setAttrInt c "width" x')
  bindDynamic
    hdynamic
    (\x' -> Snap.setAttrInt c "height" x')
  bindDynamic tdynamic (\t' -> Snap.setAttrStr c t')
  ch <- changeEvent c
  pure (Textbox c ch)
