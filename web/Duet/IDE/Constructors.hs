{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Duet.IDE.Constructors where

-- |

import           Duet.IDE.Types
import           Duet.Types
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- AST constructors

newExpression :: IO (Expression Ignore Identifier Label)
newExpression = do
  uuid <- Flux.Persist.generateUUID
  pure (ConstantExpression (Label {labelUUID = uuid}) (Identifier "_"))

newPattern :: IO (Pattern Ignore Identifier Label)
newPattern = do
  uuid <- Flux.Persist.generateUUID
  pure (WildcardPattern (Label {labelUUID = uuid}) "_")

newVariableExpression :: String -> IO (Expression Ignore Identifier Label)
newVariableExpression name = do
  uuid <- Flux.Persist.generateUUID
  pure (VariableExpression (Label {labelUUID = uuid}) (Identifier name))

newInfixExpression
  :: Char
  -> Expression Ignore Identifier Label
  -> Expression Ignore Identifier Label
  -> IO (Expression Ignore Identifier Label)
newInfixExpression op x y = do
  uuid <- Flux.Persist.generateUUID
  uuid' <- Flux.Persist.generateUUID
  pure
    (InfixExpression
       (Label {labelUUID = uuid})
       (peelHoleArgs x)
       (pure op, VariableExpression (Label uuid') (Identifier (pure op)))
       y)

peelHoleArgs :: Expression Ignore Identifier Label -> Expression Ignore Identifier Label
peelHoleArgs =
  \case
    ApplicationExpression _ f (ConstantExpression _ (Identifier "_")) -> peelHoleArgs f
    e -> e

newApplicationExpression
  :: Expression Ignore Identifier Label
  -> Expression Ignore Identifier Label
  -> IO (Expression Ignore Identifier Label)
newApplicationExpression x y = do
  uuid <- Flux.Persist.generateUUID
  pure (ApplicationExpression (Label {labelUUID = uuid}) x y)

newIfExpression :: IO (Expression Ignore Identifier Label)
newIfExpression = do
  uuid <- Flux.Persist.generateUUID
  IfExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    newExpression <*> newExpression

newCaseExpression :: IO (Expression Ignore Identifier Label)
newCaseExpression = do
  uuid <- Flux.Persist.generateUUID
  CaseExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    fmap pure newAlternative

newLambda :: IO (Expression Ignore Identifier Label)
newLambda = do
  uuid <- Flux.Persist.generateUUID
  LambdaExpression (Label {labelUUID = uuid}) <$>
    (do (CaseAlt l p e) <- newAlternative
        pure (Alternative l [p] e))

newAlternative :: IO (CaseAlt Ignore Identifier Label)
newAlternative = do
  uuid <- Flux.Persist.generateUUID
  CaseAlt (Label {labelUUID = uuid}) <$> newPattern <*> newExpression

newParens :: IO (Expression Ignore Identifier Label)
newParens = do
  uuid <- Flux.Persist.generateUUID
  ParensExpression (Label {labelUUID = uuid}) <$> newExpression
