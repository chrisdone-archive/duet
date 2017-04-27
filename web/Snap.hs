{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Snap SVG bindings.

module Snap
   where

import           Control.Concurrent
import           Control.Exception (evaluate)
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), Value(..))
import           Data.Aeson.Types (parseMaybe)
import           Data.IORef
import qualified Data.Text as T

#ifdef __GHCJS__
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal (FromJSVal(..), ToJSVal(..), toJSVal_aeson, toJSValListOf)
import           GHCJS.Foreign.Callback
#endif

--------------------------------------------------------------------------------
-- App

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.getElementById($1)"
    js_getElementById :: JSVal -> IO JSVal
getElementById :: String -> IO JSVal
getElementById str = do v <- toJSVal str
                        js_getElementById v
#else
getElementById :: String -> IO JSVal
getElementById = undefined
#endif

--------------------------------------------------------------------------------
-- Snap object

-- | A Snap object.
data Snap = Snap JSVal JSVal

-- | Create a new Snap object.
new :: ToJSVal a => a -> IO Snap
new e = do
  j_e <- toJSVal e
  fmap (`Snap` j_e) (js_newSnap j_e)

-- | Clear the canvas.
clear :: Snap -> IO ()
clear (Snap s e) = do
  js_clear s
  js_clear_el e

setDimensions :: Snap -> (Double,Double) -> IO ()
setDimensions (Snap _ e) (x,y) =
  js_snap_set_svg_dimensions e x y

--------------------------------------------------------------------------------
-- Circles

-- | A circle object.
newtype Circle = Circle JSVal
  deriving (ToJSVal)
instance HasBBox Circle
instance HasAttr Circle
instance HasDrag Circle
instance HasClick Circle
instance HasTransform Circle
instance HasPosition Circle
instance HasGrouping Circle
instance HasHover Circle

-- | Make a new circle on the snap canvas.
circle :: Snap -> Double -> Double -> Double -> IO Circle
circle (Snap snap _) circleX circleY circleRadius =
  fmap Circle (js_circle snap circleX circleY circleRadius)

--------------------------------------------------------------------------------
-- Rects

-- | A rect object.
newtype Rect = Rect JSVal
  deriving (ToJSVal)
instance HasBBox Rect
instance HasHover Rect
instance HasAttr Rect
instance HasClick Rect
instance HasDrag Rect
instance HasTransform Rect
instance HasPosition Rect
instance HasGrouping Rect

-- | Make a new rect on the snap canvas.
rect :: Snap -> Double -> Double -> Double -> Double -> Double -> Double -> IO Rect
rect (Snap snap _) rectX rectY width height rx ry =
  fmap Rect (js_rect snap rectX rectY width height rx ry)

--------------------------------------------------------------------------------
-- Text

-- | A text object.
newtype Text = Text JSVal
  deriving (ToJSVal)
instance HasClick Text
instance HasBBox Text
instance HasAttr Text
instance HasDrag Text
instance HasTransform Text
instance HasPosition Text
instance HasGrouping Text

-- | Make a new text on the snap canvas.
text :: Snap -> Double -> Double -> String -> IO Text
text (Snap snap _) textX textY t = do
  t_j <- toJSVal t
  fmap Text (js_text snap textX textY t_j)

--------------------------------------------------------------------------------
-- Paths

-- | A path object.
newtype Path = Path JSVal
  deriving (ToJSVal)
instance HasBBox Path
instance HasClick Path
instance HasHover Path
instance HasAttr Path
instance HasPosition Path
instance HasGrouping Path

data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  } deriving (Show)

instance FromJSON Point where
  parseJSON j = do
    o <- parseJSON j
    Point <$> o .: "x" <*> o .: "y"

-- | Make a new path on the snap canvas.
path :: Snap -> String -> IO Path
path (Snap snap _) t = do
  t_j <- toJSVal t
  fmap Path (js_path snap t_j)

getTotalLength :: Path -> IO Double
getTotalLength (Path j) = js_getTotalLength j

getPointAtLength :: Path -> Double -> IO Point
getPointAtLength (Path j) p = do
  o <- js_getPointAtLength j p
  value <- fromJSVal o
  result <- evaluate (value >>= parseMaybe parseJSON)
  case result of
    Nothing -> error "Couldn't parse client rect!"
    Just r -> return r

line :: Snap -> Double -> Double -> Double -> Double -> IO Path
line snap x1 y1 x2 y2 =
  Snap.path snap (unwords ["M", show x1, show y1, "L", show x2, show y2])

--------------------------------------------------------------------------------
-- Bounding box

-- | A path object.
data BBox = BBox
  { bboxWidth :: !Double
  , bboxHeight :: !Double
  , bboxLeft :: !Double
  , bboxTop :: !Double
  , bboxRight :: !Double
  , bboxBottom :: !Double
  } deriving (Show)

instance FromJSON BBox where
  parseJSON j = do
    o <- parseJSON j
    BBox <$> o .: "width" <*> o .: "height" <*> o .: "x" <*> o .: "y" <*>
      o .: "x2" <*>
      o .: "y2"

class ToJSVal a => HasBBox a

-- | Make a new getBBox on the snap canvas.
getBBox :: HasBBox object => object -> IO BBox
getBBox t = do
  t_j <- toJSVal t
  o <- js_getBBox t_j
  value <- fromJSVal o
  result <- evaluate (value >>= parseMaybe parseJSON)
  case result of
    Nothing -> error "Couldn't parse client rect!"
    Just r -> return r

--------------------------------------------------------------------------------
-- Attributes (font, fill, stroke, etc.)

class ToJSVal a => HasAttr a

-- | Apply attributes to the object.
attr :: HasAttr object => object -> Value -> IO ()
attr o attrs = do
  o_j <- toJSVal o
  v <- toJSVal_aeson attrs
  js_attr o_j v

-- | Apply one dataibute to the object.
setData :: (HasAttr object,ToJSVal v) => object -> String -> v -> IO ()
setData o key val' = do
  o_j <- toJSVal o
  k <- toJSVal key
  v <- toJSVal val'
  js_setData o_j k v

-- | Apply one attribute to the object.
setAttr :: (HasAttr object,ToJSVal v) => object -> String -> v -> IO ()
setAttr o key val' = do
  o_j <- toJSVal o
  k <- toJSVal key
  v <- toJSVal val'
  js_setAttr o_j k v

--------------------------------------------------------------------------------
-- Positioning

class ToJSVal a => HasPosition a

-- | Reposition something after something else.
after :: (HasPosition a,HasPosition b) => a -> b -> IO ()
after this that = do
  a <- toJSVal this
  b <- toJSVal that
  js_after a b

--------------------------------------------------------------------------------
-- Grouping

class ToJSVal a => HasGrouping a

newtype Group = Group JSVal
  deriving (ToJSVal)
instance HasGrouping Group
instance HasDrag Group
instance HasBBox Group
instance HasClick Group
instance HasTransform Group
instance HasHover Group
instance HasAttr Group

group :: Snap -> [Some HasGrouping] -> IO Group
group (Snap snap _) xs = do
  xs' <- mapM (\(Some x) -> toJSVal x) xs >>= toJSVal
  fmap Group (js_group snap xs')

--------------------------------------------------------------------------------
-- Dragging

class ToJSVal a => HasDrag a

drag
  :: HasDrag a
  => a -- ^ Draggable thing.
  -> (Double -> Double -> IO ()) -- ^ During drag.
  -> (Double -> Double -> IO ()) -- ^ Start drag.
  -> (Bool -> IO ()) -- ^ End drag.
  -> IO ()
drag d during start end = do
  duringCallback <-
    asyncCallback2
      (\x y -> do
         Just x' <- fromJSVal x
         Just y' <- fromJSVal y
         during x' y')
  startCallback <-
    asyncCallback2
      (\x y -> do
         Just x' <- fromJSVal x
         Just y' <- fromJSVal y
         start x' y')
  endCallback <-
    asyncCallback1
      (\dragged -> do
         Just v <- fromJSVal dragged
         end v)
  d_j <- toJSVal d
  js_drag d_j duringCallback startCallback endCallback

--------------------------------------------------------------------------------
-- Hover

class ToJSVal a => HasHover a

hover :: HasHover a => a -> IO () -> IO () -> IO ()
hover o on off = do
  j <- toJSVal o
  onx <- asyncCallback on
  offx <- asyncCallback off
  js_hover j onx offx

--------------------------------------------------------------------------------
-- Click

class ToJSVal a => HasClick a

newtype Event = Event
  { eventEvent :: JSVal
  }

stopPropagation :: Event -> IO ()
stopPropagation (Event e) = js_stopPropagation e

data Modifier
  = AltKey
  | NoModifier
             deriving (Show)

-- | Handle the click event, if another click comes in under 500ms,
-- then trigger a double-click instead.
singleClick
  :: HasClick a
  => a
  -> (Event -> Modifier -> Double -> Double -> IO ()) -- Click
  -> IO ()
singleClick o onClick = do
  j <- toJSVal o
  onx <-
    asyncCallback3
      (\e xy' alt' -> do
         Just [x, y] <- fromJSVal xy'
         Just alt <- fromJSVal alt'
         onClick
           (Event e)
           (if alt
              then AltKey
              else NoModifier)
           x
           y)
  js_click j onx

-- | Handle the click event, if another click comes in under 500ms,
-- then trigger a double-click instead.
bothClicks
  :: HasClick a
  => a
  -> (Event -> Modifier -> Double -> Double -> IO ()) -- Click
  -> (Event -> Modifier -> Double -> Double -> IO ()) -- Double click
  -> IO ()
bothClicks o onClick onDblClick = do
  j <- toJSVal o
  clicks <- newIORef (0 :: Int)
  mtid <- newIORef Nothing
  onx <-
    asyncCallback3
      (\e xy' alt' -> do
         modifyIORef' clicks (+ 1)
         clickCount <- readIORef clicks
         Just [x, y] <- fromJSVal xy'
         Just alt <- fromJSVal alt'
         if clickCount > 1
           then do
             readIORef mtid >>= maybe (return ()) killThread
             writeIORef clicks 0
             onDblClick
               (Event e)
               (if alt
                  then AltKey
                  else NoModifier)
               x
               y
           else do
             tid <-
               forkIO
                 (do threadDelay (1000 * 200)
                     writeIORef clicks 0
                     onClick
                       (Event e)
                       (if alt
                          then AltKey
                          else NoModifier)
                       x
                       y)
             writeIORef mtid (Just tid))
  js_click j onx

--------------------------------------------------------------------------------
-- Matrix transforms

newtype Matrix = Matrix JSVal

class ToJSVal a => HasTransform a

newMatrix :: IO Matrix
newMatrix = fmap Matrix js_newMatrix

translate :: Matrix -> Double -> Double -> IO ()
translate (Matrix m) x y = js_translate m x y

transform :: HasTransform a => a -> Matrix -> IO ()
transform a (Matrix m) = do
  j <- toJSVal a
  js_transform j m

--------------------------------------------------------------------------------
-- Text boxes

newtype Textbox = Textbox JSVal

-- | Create a text box for the user to write into.
textbox :: Snap -> Double -> Double -> Double -> Double -> String -> IO Textbox
textbox (Snap _ e) x y w h t = do
  t_j <- toJSVal t
  fmap Textbox (js_textbox e x y w h t_j)

val :: Textbox -> IO T.Text
val (Textbox j) = do
  r <- js_val j
  t <- fromJSVal r
  case t of
    Nothing -> return ""
    Just x -> return x

keyup :: Textbox -> (Int -> T.Text -> IO ()) -> IO ()
keyup (Textbox j) cont = do
  callback <-
    asyncCallback2
      (\key val' -> do
         Just v <- fromJSVal val'
         Just k <- fromJSVal key
         cont k (T.pack v))
  js_keyup j callback

keydown :: Textbox -> (Int -> T.Text -> IO ()) -> IO ()
keydown (Textbox j) cont = do
  callback <-
    asyncCallback2
      (\key val' -> do
         Just v <- fromJSVal val'
         Just k <- fromJSVal key
         cont k (T.pack v))
  js_keydown j callback

change :: Textbox -> (String -> IO ()) -> IO ()
change (Textbox j) cont = do
  callback <-
    asyncCallback1
      (\val' -> do
         Just v <- fromJSVal val'
         cont v)
  js_change j callback

setAttrInt :: Textbox -> String -> Double -> IO ()
setAttrInt (Textbox j) key val = do
  k' <- toJSVal key
  js_setAttrInt j k' val

setAttrStr :: Textbox -> String -> IO ()
setAttrStr (Textbox j) val = do
  val' <- toJSVal val
  js_setAttrStr j val'

#ifdef __GHCJS__
foreign import javascript unsafe
  "snap_textbox($1, $2, $3, $4, $5, $6)"
  js_textbox :: JSVal -> Double -> Double -> Double -> Double -> JSVal -> IO JSVal
#else
js_textbox :: JSVal -> Double -> Double -> Double -> Double -> JSVal -> IO JSVal
js_textbox = undefined
#endif

--------------------------------------------------------------------------------
-- Generalized constrained heterogeneity

data Some ctx = forall a. ctx a => Some a

--------------------------------------------------------------------------------
-- Foreign imports

#ifdef __GHCJS__
foreign import javascript unsafe
  "$($1).attr({ width: $2, height: $3 })"
  js_snap_set_svg_dimensions :: JSVal -> Double -> Double -> IO ()
#else
js_snap_set_svg_dimensions :: JSVal -> Double -> Double -> IO ()
js_snap_set_svg_dimensions = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "Snap($1)"
  js_newSnap :: JSVal -> IO JSVal
#else
js_newSnap :: JSVal -> IO JSVal
js_newSnap = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).stopPropagation()"
  js_stopPropagation :: JSVal -> IO ()
#else
js_stopPropagation :: JSVal -> IO ()
js_stopPropagation = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).clear()"
  js_clear :: JSVal -> IO ()
#else
js_clear :: JSVal -> IO ()
js_clear = undefined
#endif
#ifdef __GHCJS__
foreign import javascript unsafe
  "$($1).parent().children('input').remove()"
  js_clear_el :: JSVal -> IO ()
#else
js_clear_el :: JSVal -> IO ()
js_clear_el = undefined
#endif


#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).circle($2, $3, $4)"
  js_circle :: JSVal -> Double -> Double -> Double -> IO JSVal
#else
js_circle :: JSVal -> Double -> Double -> Double -> IO JSVal
js_circle = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).rect($2, $3, $4, $5, $6, $7)"
  js_rect :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO JSVal
#else
js_rect :: JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO JSVal
js_rect = undefined
#endif


#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).text($2, $3, $4)"
  js_text :: JSVal -> Double -> Double -> JSVal -> IO JSVal
#else
js_text :: JSVal -> Double -> Double -> JSVal -> IO JSVal
js_text = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).path($2)"
  js_path :: JSVal -> JSVal -> IO JSVal
#else
js_path :: JSVal -> JSVal -> IO JSVal
js_path = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).getBBox()"
  js_getBBox :: JSVal -> IO JSVal
#else
js_getBBox :: JSVal -> IO JSVal
js_getBBox = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).attr($2)"
  js_attr :: JSVal -> JSVal -> IO ()
#else
js_attr :: JSVal ->  JSVal -> IO ()
js_attr = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var attrs = {}; attrs[$2] = $3; ($1).attr(attrs)"
  js_setAttr :: JSVal -> JSVal -> JSVal -> IO ()
#else
js_setAttr :: JSVal -> JSVal -> JSVal -> IO ()
js_setAttr = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var attrs = {}; attrs[$2] = $3; $($1).attr(attrs)"
  js_setAttrInt :: JSVal -> JSVal -> Double -> IO ()
#else
js_setAttrInt :: JSVal -> JSVal -> Double -> IO ()
js_setAttrInt = undefined
#endif
#ifdef __GHCJS__
foreign import javascript unsafe
  "$($1).val($2)"
  js_setAttrStr :: JSVal -> JSVal -> IO ()
#else
js_setAttrStr :: JSVal -> JSVal -> IO ()
js_setAttrStr = undefined
#endif
#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).data($2, $3)"
  js_setData :: JSVal -> JSVal -> JSVal -> IO ()
#else
js_setData :: JSVal -> JSVal -> JSVal -> IO ()
js_setData = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).after($2)"
  js_after :: JSVal -> JSVal -> IO ()
#else
js_after :: JSVal ->  JSVal -> IO ()
js_after = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function(){ return ($1).group.apply($1,$2); })()"
  js_group :: JSVal -> JSVal -> IO JSVal
#else
js_group :: JSVal -> JSVal -> IO JSVal
js_group = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function(){ var dragged = false; ($1).drag(function(x,y){ ($2)(x,y); dragged = true},function(x,y,e){$3(e.offsetX,e.offsetY)},function(){ ($4)(dragged) }); })()"
  js_drag :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> Callback (JSVal -> JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()
#else
js_drag :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> Callback (JSVal -> JSVal ->  IO ()) -> Callback (JSVal -> IO ()) -> IO ()
js_drag = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "new Snap.Matrix()"
  js_newMatrix :: IO JSVal
#else
js_newMatrix :: IO JSVal
js_newMatrix = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).translate($2,$3)"
  js_translate :: JSVal -> Double -> Double -> IO ()
#else
js_translate :: JSVal -> Double -> Double -> IO ()
js_translate = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).transform($2)"
  js_transform :: JSVal -> JSVal -> IO ()
#else
js_transform :: JSVal -> JSVal -> IO ()
js_transform = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).hover($2, $3)"
  js_hover :: JSVal -> Callback (IO ()) -> Callback (IO ()) -> IO ()
#else
js_hover :: JSVal -> Callback (IO ()) -> Callback (IO ()) -> IO ()
js_hover = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "Snap.path.getPointAtLength($1,$2)"
  js_getPointAtLength :: JSVal -> Double -> IO JSVal
#else
js_getPointAtLength :: JSVal -> Double -> IO JSVal
js_getPointAtLength = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.getTotalLength()"
  js_getTotalLength :: JSVal -> IO Double
#else
js_getTotalLength :: JSVal -> IO Double
js_getTotalLength = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.val()"
  js_val :: JSVal -> IO JSVal
#else
js_val :: JSVal -> IO JSVal
js_val = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.keydown(function(e){ $2(e.which, $1.val()); })"
  js_keydown :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
#else
js_keydown :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
js_keydown = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.keyup(function(e){ $2(e.which, $1.val()); })"
  js_keyup :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
#else
js_keyup :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
js_keyup = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.change(function(e){ $2($1.val()); })"
  js_change :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
#else
js_change :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
js_change = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "($1).click(function(e){ $2(e, [e.offsetX, e.offsetY], e.altKey); })"
  js_click :: JSVal -> Callback (JSVal -> JSVal -> JSVal -> IO ()) -> IO ()
#else
js_click :: JSVal -> Callback (JSVal -> JSVal -> JSVal -> IO ()) -> IO ()
js_click = undefined
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log('%o',$1)"
  js_console_log :: JSVal -> IO ()
#else
js_console_log :: JSVal -> IO ()
js_console_log = undefined
#endif

--------------------------------------------------------------------------------
-- GHCJS compat

#ifndef __GHCJS__
class ToJSVal a where toJSVal :: a -> IO JSVal
instance ToJSVal JSVal where toJSVal = undefined
instance ToJSVal Char where toJSVal = undefined
instance ToJSVal Int where toJSVal = undefined
instance ToJSVal Double where toJSVal = undefined
instance (ToJSVal a) => ToJSVal [a] where toJSVal = undefined
class FromJSVal a where fromJSVal :: JSVal -> IO a
instance FromJSVal String where fromJSVal = undefined
instance FromJSVal Int where fromJSVal = undefined
instance FromJSVal Bool where fromJSVal = undefined
instance FromJSVal (Maybe a) where fromJSVal = undefined
toJSVal_aeson :: ToJSON a => a -> IO JSVal
toJSVal_aeson = undefined
data JSVal
data Callback a
asyncCallback :: (IO ()) -> IO (Callback (IO ()))
asyncCallback = undefined
asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
asyncCallback2 = undefined
asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
asyncCallback3 = undefined
asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
asyncCallback1 = undefined
#endif
