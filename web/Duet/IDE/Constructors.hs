{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Duet.IDE.Constructors where

-- |

import           Duet.IDE.Types
import           Duet.Types
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- AST constructors

newExpression :: IO (Expression UnkindedType Identifier Label)
newExpression = do
  uuid <- Flux.Persist.generateUUID
  pure (ConstantExpression (Label {labelUUID = uuid}) (Identifier "_"))

newPattern :: IO (Pattern UnkindedType Identifier Label)
newPattern = do
  uuid <- Flux.Persist.generateUUID
  pure (WildcardPattern (Label {labelUUID = uuid}) "_")

newVariableExpression :: String -> IO (Expression UnkindedType Identifier Label)
newVariableExpression name = do
  uuid <- Flux.Persist.generateUUID
  pure (VariableExpression (Label {labelUUID = uuid}) (Identifier name))

newInfixExpression
  :: Char
  -> Expression UnkindedType Identifier Label
  -> Expression UnkindedType Identifier Label
  -> IO (Expression UnkindedType Identifier Label)
newInfixExpression op x y = do
  uuid <- Flux.Persist.generateUUID
  uuid' <- Flux.Persist.generateUUID
  pure
    (InfixExpression
       (Label {labelUUID = uuid})
       x
       (pure op, VariableExpression (Label uuid') (Identifier (pure op)))
       y)

newApplicationExpression
  :: Expression UnkindedType Identifier Label
  -> Expression UnkindedType Identifier Label
  -> IO (Expression UnkindedType Identifier Label)
newApplicationExpression x y = do
  uuid <- Flux.Persist.generateUUID
  pure (ApplicationExpression (Label {labelUUID = uuid}) x y)

newIfExpression :: IO (Expression UnkindedType Identifier Label)
newIfExpression = do
  uuid <- Flux.Persist.generateUUID
  IfExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    newExpression <*> newExpression

newCaseExpression :: IO (Expression UnkindedType Identifier Label)
newCaseExpression = do
  uuid <- Flux.Persist.generateUUID
  CaseExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    fmap pure newAlternative

newLambda :: IO (Expression UnkindedType Identifier Label)
newLambda = do
  uuid <- Flux.Persist.generateUUID
  LambdaExpression (Label {labelUUID = uuid}) <$>
    (do (CaseAlt l p e) <- newAlternative
        pure (Alternative l [p] e))

newAlternative :: IO (CaseAlt UnkindedType Identifier Label)
newAlternative = do
  uuid <- Flux.Persist.generateUUID
  CaseAlt (Label {labelUUID = uuid}) <$> newPattern <*> newExpression

newParens :: IO (Expression UnkindedType Identifier Label)
newParens = do
  uuid <- Flux.Persist.generateUUID
  ParensExpression (Label {labelUUID = uuid}) <$> newExpression

newBindDecl :: IO (Flux.Persist.UUID,Decl UnkindedType Identifier Label)
newBindDecl = do
  bgd <- fmap Label Flux.Persist.generateUUID
  implicitBinding <- fmap Label Flux.Persist.generateUUID
  implicitBindingId <- Flux.Persist.generateUUID
  alternativeId <- fmap Label Flux.Persist.generateUUID
  expr <- newExpression
  pure
    ( implicitBindingId
    , BindDecl
        bgd
        (ImplicitBinding
           (ImplicitlyTypedBinding
            { implicitlyTypedBindingLabel = implicitBinding
            , implicitlyTypedBindingId =
                (Identifier "_", Label implicitBindingId)
            , implicitlyTypedBindingAlternatives =
                [ Alternative
                  { alternativeLabel = alternativeId
                  , alternativePatterns = []
                  , alternativeExpression = expr
                  }
                ]
            })))
