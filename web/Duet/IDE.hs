{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Duet.IDE where

import           Control.Concurrent
import           Control.Monad.State (execStateT)
import           Duet.IDE.Interpreters
import           Duet.IDE.Types
import           Duet.Types
import           React.Flux (ReactStore, SomeStoreAction)
import qualified React.Flux as Flux
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- Store setup

-- | Dispatch an action on the store.
dispatch :: Action -> SomeStoreAction
dispatch a = Flux.SomeStoreAction store a

-- | The app's model.
store :: ReactStore State
store = do
  Flux.mkStore
    State
    { stateCursor = Cursor {cursorUUID = uuidI}
    , stateAST =
        DeclNode
          (BindGroupDecl
             (Label {labelUUID = uuidD})
             (BindGroup
              { bindGroupImplicitlyTypedBindings =
                  [ [ ImplicitlyTypedBinding
                      { implicitlyTypedBindingLabel =
                          Label (Flux.Persist.UUID "STARTER-BINDING")
                      , implicitlyTypedBindingId = (Identifier "_",Label uuidI)
                      , implicitlyTypedBindingAlternatives =
                          [ Alternative
                            { alternativeLabel = Label (Flux.Persist.UUID "STARTER-ALT")
                            , alternativePatterns = []
                            , alternativeExpression =
                                ConstantExpression
                                  (Label {labelUUID = uuidE})
                                  (Identifier "_")
                            }
                          ]
                      }
                    ]
                  ]
              , bindGroupExplicitlyTypedBindings = []
              }))
    }
  where
    uuidE = Flux.Persist.UUID "STARTER-EXPR"
    uuidD = Flux.Persist.UUID "STARTER-DECL"
    uuidI = Flux.Persist.UUID "STARTER-BINDING-ID"

--------------------------------------------------------------------------------
-- Model

instance Flux.StoreData State where
  type StoreAction State = Action
  transform action state = do
    putStrLn ("Action: " ++ show action)
    -- putStrLn ("State before: " ++ show state)
    state' <- execStateT (interpretAction action) state
    -- putStrLn ("State after: " ++ show state')
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    pure state'
