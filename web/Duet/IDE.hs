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
    initState

-- | Initial state of the app.
initState :: State
initState = makeState initName initExpression

initName :: String
initName = "_"

initExpression :: forall (t :: * -> *) i. Expression t i Label
initExpression =
  (ConstantExpression (Label {labelUUID = starterExprUUID}) (Identifier "_"))

starterExprUUID :: Flux.Persist.UUID
starterExprUUID = Flux.Persist.UUID "STARTER-EXPR"

makeState :: String -> Expression Ignore Identifier Label -> State
makeState ident expr =
  State
  { stateCursor = Cursor {cursorUUID = uuidI}
  , stateAST =
      ModuleNode
        (Label (Flux.Persist.UUID "STARTER-MODULE"))
        [ BindGroupDecl
            (Label {labelUUID = uuidD})
            (BindGroup
             { bindGroupImplicitlyTypedBindings =
                 [ [ ImplicitlyTypedBinding
                     { implicitlyTypedBindingLabel =
                         Label (Flux.Persist.UUID "STARTER-BINDING")
                     , implicitlyTypedBindingId =
                         (Identifier ident, Label uuidI)
                     , implicitlyTypedBindingAlternatives =
                         [ Alternative
                           { alternativeLabel =
                               Label (Flux.Persist.UUID "STARTER-ALT")
                           , alternativePatterns = []
                           , alternativeExpression = expr
                           }
                         ]
                     }
                   ]
                 ]
             , bindGroupExplicitlyTypedBindings = []
             })
        ]
  }
  where
    uuidD = Flux.Persist.UUID "STARTER-DECL"
    uuidI = Flux.Persist.UUID "STARTER-BINDING-ID"

--------------------------------------------------------------------------------
-- Model

instance Flux.StoreData State where
  type StoreAction State = Action
  transform action state = do
    -- putStrLn ("Action: " ++ show action)
    -- putStrLn ("State before: " ++ show state)
    state' <- execStateT (interpretAction action) state
    -- putStrLn ("State after: " ++ show state')
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    pure state'
