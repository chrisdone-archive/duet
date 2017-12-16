{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Duet.IDE where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.State (execStateT)
import           Control.Monad.Supply
import           Data.Typeable
import           Duet.Context
import           Duet.Errors
import           Duet.IDE.Interpreters
import           Duet.IDE.Types
import           Duet.Infer
import           Duet.Resolver
import           Duet.Types
import           React.Flux (ReactStore, SomeStoreAction)
import qualified React.Flux as Flux
import qualified React.Flux.Persist as Flux.Persist
import           Shared

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

makeState :: String -> Expression UnkindedType Identifier Label -> State
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
    state' <- execStateT (interpretAction action) state
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    _ <-
      forkIO
        (do result <-
              catch
                (fmap
                   Right
                   (evalSupplyT
                      (do (binds, context) <-
                            createContext
                              (case stateAST state' of
                                 ModuleNode _ ds -> ds
                                 _ -> [])
                          pure (binds, context))
                      [1 ..]))
                (\e@(ContextException {}) -> pure (Left e))
            either (putStrLn . displayException) (const (pure ())) result)
    pure state'

--------------------------------------------------------------------------------
-- Context setup

data ContextException = ContextException (SpecialTypes Name) SomeException
  deriving (Show, Typeable)

instance Exception ContextException where
  displayException (ContextException specialTypes (SomeException se)) =
    maybe
      (maybe
         (maybe
            (maybe
               (maybe
                  (displayException se)
                  (displayRenamerException specialTypes)
                  (cast se))
               (displayInferException specialTypes)
               (cast se))
            (displayStepperException specialTypes)
            (cast se))
         (displayResolveException specialTypes)
         (cast se))
      displayParseException
      (cast se)

-- | Create a context of all renamed, checked and resolved code.
createContext
  :: (MonadSupply Int m, MonadIO m, MonadCatch m)
  => [Decl UnkindedType Identifier Label]
  -> m ([BindGroup Type Name (TypeSignature Type Name Label)], Context Type Name Label)
createContext decls = do
  do builtins <-
       setupEnv mempty [] >>=
       traverse
         (const
            (do uuid <- liftIO Flux.Persist.generateUUID
                pure (Label {labelUUID = uuid})))
     let specials = builtinsSpecials builtins
     catch
       (do (typeClasses, signatures, renamedBindings, scope, dataTypes) <-
             renameEverything decls specials builtins
           -- Type class definition
           addedTypeClasses <- addClasses builtins typeClasses
               -- Type checking
           (bindGroups, typeCheckedClasses) <-
             typeCheckModule
               addedTypeClasses
               signatures
               (builtinsSpecialTypes builtins)
               renamedBindings
           -- Type class resolution
           resolvedTypeClasses <-
             resolveTypeClasses
               typeCheckedClasses
               (builtinsSpecialTypes builtins)
           resolvedBindGroups <-
             mapM
               (resolveBindGroup
                  resolvedTypeClasses
                  (builtinsSpecialTypes builtins))
               bindGroups
           -- Create a context of everything
           let context =
                 Context
                 { contextSpecialSigs = builtinsSpecialSigs builtins
                 , contextSpecialTypes = builtinsSpecialTypes builtins
                 , contextSignatures = signatures
                 , contextScope = scope
                 , contextTypeClasses = resolvedTypeClasses
                 , contextDataTypes = dataTypes
                 }
           pure (resolvedBindGroups, context))
       (throwM . ContextException (builtinsSpecialTypes builtins))
