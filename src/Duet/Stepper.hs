{-# LANGUAGE LambdaCase #-}
-- |

module Duet.Stepper where

import Control.Monad.Catch
import Data.Maybe
import Data.Typeable
import Duet.Types


data StepException
  = CouldntFindName !Name
  | CouldntFindNameByString !String
  deriving (Typeable, Show)
instance Exception StepException

stepper
  :: MonadThrow m
  => [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> String
  -> m (Expression Name (TypeSignature Name Location))
stepper binds string =
  do e <- lookupNameByString string binds
     expand e binds

expand
  :: MonadThrow m
  => Expression Name (TypeSignature Name Location)
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
expand e b = go e
  where
    go x =
      case x of
        VariableExpression _ i -> do
          e' <- lookupName i b
          pure e'
        LiteralExpression {} -> return x
        ConstantExpression {} -> return x
        ApplicationExpression l func arg ->
          case func of
            LambdaExpression l0 (Alternative l' params body) ->
              case params of
                (VariablePattern param:params') ->
                  let body' = substitute param arg body
                  in case params' of
                       [] -> pure body'
                       _ -> pure (LambdaExpression l0 (Alternative l' params' body'))
                [] -> error "Unsupported lambda."
            _ -> do
              func' <- expand func b
              pure (ApplicationExpression l func' arg)
        InfixExpression {} -> return x
        LetExpression {} -> return x
        LambdaExpression {} -> return x
        IfExpression {} -> return x
        CaseExpression {} -> return x

substitute :: Name -> Expression Name l -> Expression Name l -> Expression Name l
substitute i arg =
  \case
    VariableExpression l i'
      | i == i' -> arg
      | otherwise -> VariableExpression l i'
    ApplicationExpression l f x ->
      ApplicationExpression l (substitute i arg f) (substitute i arg x)
    InfixExpression l x f y ->
      InfixExpression l (substitute i arg x) f (substitute i arg y)
    LetExpression {} -> error "let expressions unsupported."
    CaseExpression {} -> error "case expressions unsupported."
    IfExpression l a b c ->
      IfExpression
        l
        (substitute i arg a)
        (substitute i arg b)
        (substitute i arg c)
    x@LiteralExpression {} -> x
    LambdaExpression l (Alternative l' args body) ->
      LambdaExpression l (Alternative l' args (substitute i arg body))

lookupName
  :: (MonadThrow m)
  => Name
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
lookupName identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindName identifier)
    Just i -> pure i
  where
    findIdent (BindGroup _ is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ i [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is))

lookupNameByString
  :: (MonadThrow m)
  => String
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
lookupNameByString identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindNameByString identifier)
    Just i -> pure i
  where
    findIdent (BindGroup _ is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ (NameFromSource _ i) [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is))
