{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}

-- |

module Duet.IDE.Types where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Data
import           Data.Map.Strict (Map)
import           Duet.Types
import           GHC.Generics

data State = State
  { stateCursor :: !Cursor
  , stateAST :: !Node
  , stateTypeCheck :: Either String ()
  , stateHighlightErrors :: !(Map UUID String)
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Node
  = ExpressionNode !(Expression UnkindedType Identifier Label)
  | DeclNode !(Decl UnkindedType Identifier Label)
  | ModuleNode !Label ![Decl UnkindedType Identifier Label]
  | NameNode !(Identifier, Label)
  | OperatorNode !Label !Identifier
  | PatternNode !(Pattern UnkindedType Identifier Label)
  | AltNode !(CaseAlt UnkindedType Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

nodeUUID :: Node -> UUID
nodeUUID = labelUUID . nodeLabel

nodeLabel :: Node -> Label
nodeLabel =
  \case
    ExpressionNode e -> expressionLabel e
    ModuleNode l _ -> l
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
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Keydown
  = BackspaceKey
  | TabKey
  | DownKey
  | UpKey
  | LeftKey
  | RightKey
  | ReturnKey
  deriving (Generic, NFData, Show, FromJSON, ToJSON)
