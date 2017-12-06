{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}

-- |

module Duet.IDE.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Data
import Duet.Printer (PrintableType(..))
import Duet.Types
import GHC.Generics
import React.Flux.Persist (UUID)

data Ignore a = Ignore
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

instance PrintableType Ignore where
  printType _ _ _ = ""

data State = State
  { stateCursor :: !Cursor
  , stateAST :: !Node
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Node
  = ExpressionNode !(Expression Ignore Identifier Label)
  | DeclNode !(Decl Ignore Identifier Label)
  | NameNode !(Identifier, Label)
  | OperatorNode !Label !Identifier
  | PatternNode !(Pattern Ignore Identifier Label)
  | AltNode !(CaseAlt Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

nodeUUID :: Node -> UUID
nodeUUID = labelUUID . nodeLabel

nodeLabel :: Node -> Label
nodeLabel =
  \case
    ExpressionNode e -> expressionLabel e
    DeclNode d -> declLabel d
    NameNode (_, d) -> d
    OperatorNode l _ -> l
    PatternNode p -> patternLabel p
    AltNode c -> caseAltLabel c

data Cursor = Cursor
  { cursorUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Action
  = ReplaceState !State
  | KeyDown !Bool !Keydown
  | KeyPress !Int
  | InsertChar !Char
  | PutExpression !(Expression Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Label = Label
  { labelUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

data Keydown
  = BackspaceKey
  | TabKey
  | DownKey
  | UpKey
  | LeftKey
  | RightKey
  | ReturnKey
  | OpenParens
  | CloseParens
  deriving (Generic, NFData, Show, FromJSON, ToJSON)
