{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module React.Flux.Persist where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal (FromJSVal(..), ToJSVal(..), toJSVal_aeson)
import GHCJS.Types (JSVal, JSString)
import Data.JSString

foreign import javascript unsafe
    "(function(){ if (sessionStorage.getItem($1)) return JSON.parse(sessionStorage.getItem($1)); })()"
    js_sessionStorage_getItemVal :: JSString -> IO JSVal

foreign import javascript unsafe
    "sessionStorage.setItem($1,JSON.stringify($2));"
    js_sessionStorage_setItemVal :: JSString -> JSVal -> IO ()

-- | Get the app state.
getAppStateVal :: FromJSON a => IO (Maybe a)
getAppStateVal = do
  jv <- js_sessionStorage_getItemVal "app-state"
  value <- fromJSVal jv
  evaluate (value >>= parseMaybe parseJSON)
  where
    eitherToMaybe = either (const Nothing) Just

-- | Set the app state.
setAppStateVal
  :: ToJSON a
  => a -> IO ()
setAppStateVal app = do
  _ <-
    forkIO
      (do !val <- toJSVal_aeson app
          js_sessionStorage_setItemVal "app-state" val)
  return ()

foreign import javascript unsafe "window['generateUUID']()"
    js_generateUUID :: IO JSString

generateUUID :: IO JSString
generateUUID = -- unpack <$>
  js_generateUUID
