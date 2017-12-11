{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Record IDE tests.

module Main where

import           Control.Monad
import           Data.Generics
import           Data.IORef
import           Duet.IDE
import           Duet.IDE.Types
import           Duet.IDE.View
import           Duet.IDE.Test
import           Duet.Types
import qualified React.Flux as Flux
import qualified React.Flux.Events as Flux.Events

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  Flux.alterStore store (ReplaceState (rhsSelectedState initExpression))
  keys <- newIORef []
  let dumpTest = do
        state <- Flux.getStoreData store
        keys' <- readIORef keys
        putStrLn
          (unwords
             [ "Test " ++ show ""
             , show
                 (map
                    (\case
                       KeyPress n -> KeyPressAction (toEnum (fromIntegral n))
                       KeyDown bool keydown -> KeyDownAction bool keydown
                       event ->
                         error ("Can't translate this event: " ++  show event))
                    keys')
             , "(focus (" ++ show (cursorUUID (stateCursor state)) ++ ") (rhsSelectedState (" ++
               show
                 (head
                    (listify
                       (const True :: Expression Ignore Identifier Label -> Bool)
                       (stateAST state))) ++
               ")))"
             ])
  Flux.Events.onBodyKeydown
    (\shift key -> do
       event <-
         case key of
           8 -> (pure . Just) (KeyDown shift BackspaceKey)
           9 -> (pure . Just) (KeyDown shift TabKey)
           37 -> (pure . Just) (KeyDown shift LeftKey)
           39 -> (pure . Just) (KeyDown shift RightKey)
           38 -> (pure . Just) (KeyDown shift UpKey)
           40 -> (pure . Just) (KeyDown shift DownKey)
           13 -> (pure . Just) (KeyDown shift ReturnKey)
           _ -> pure Nothing
       maybe
         (pure ())
         (\ev -> do
            modifyIORef keys (++ [ev])
            Flux.alterStore store ev
            dumpTest)
         event)
  Flux.Events.onBodyKeypress
    (\key -> do
       when False (print ("keypress", key))
       modifyIORef keys (++ [KeyPress key])
       Flux.alterStore store (KeyPress key)
       dumpTest)
  Flux.reactRender
    "app"
    (Flux.defineControllerView
       "State"
       store
       (\state () -> renderModule (stateCursor state) (stateAST state)))
    ()
