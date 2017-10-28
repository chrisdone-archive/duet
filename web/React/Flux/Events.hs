-- |

module React.Flux.Events where

import           Control.Concurrent
import           Control.DeepSeq
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal (FromJSVal(..), ToJSVal(..), toJSVal_aeson)
import           GHCJS.Types (JSVal, JSString)
import           React.Flux ((@=))
import           React.Flux (ReactStore, ViewEventHandler, SomeStoreAction, ReactView)
import qualified React.Flux as Flux
import           React.Flux.Internal (ReactElementM)
import qualified React.Flux.Lifecycle as Flux.Lifecycle
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- Event handling additions

foreign import javascript unsafe
    "$($1).draggable({ stop: function(_,o){ $3(o.offset.left,o.offset.top) }, helper: 'clone', revert: $2, revertDuration: 0 })"
    js_draggable :: JSVal -> Bool -> Callback (JSVal -> JSVal -> IO ()) -> IO ()

onDrag :: JSVal -> Bool -> (Double -> Double -> IO ()) -> IO ()
onDrag el revert cont = do
  callback <-
    asyncCallback2
      (\x' y' -> do
         Just x <- fromJSVal x'
         Just y <- fromJSVal y'
         cont x y)
  js_draggable el revert callback

foreign import javascript unsafe
    "jQuery(document.body).keydown(function(e){if(e.target==document.body) { if (e.which==8||e.which==9||e.which==37||e.which==38||e.which==39||e.which==40) e.preventDefault(); $1(e.shiftKey, e.which); }});"
    js_Body_Keydown :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

-- | Do something when there's a keydown in the body (not in an input element or whatnot).
onBodyKeydown :: (Bool -> Int -> IO ()) ->  IO ()
onBodyKeydown cont = do
  callback <-
    asyncCallback2
      (\shiftj jsval -> do
         i <- fromJSVal jsval
         shift <- fromJSVal shiftj
         case i of
           Just x -> cont (shift == Just True) x
           Nothing -> return ())
  js_Body_Keydown callback

foreign import javascript unsafe
    "jQuery(document.body).keypress(function(e){if(e.target==document.body)$1(e.which);});"
    js_Body_Keypress :: Callback (JSVal -> IO ()) -> IO ()

-- | Do something when there's a keypress in the body (not in an input element or whatnot).
onBodyKeypress :: (Int -> IO ()) ->  IO ()
onBodyKeypress cont = do
  callback <-
    asyncCallback1
      (\jsval -> do
         i <- fromJSVal jsval
         case i of
           Just x -> cont x
           Nothing -> return ())
  js_Body_Keypress callback
