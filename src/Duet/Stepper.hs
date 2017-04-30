{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- |

module Duet.Stepper where

import Control.Monad.Catch
import Data.List
import Data.Maybe
import Duet.Types

expand
  :: MonadThrow m
  => SpecialSigs Name
  -> [TypeSignature Name Name]
  -> Expression Name (TypeSignature Name Location)
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
expand specialSigs signatures e b = go e
  where
    go x =
      case x of
        VariableExpression loc i -> do
          case find ((== i) . typeSignatureA) signatures of
            Nothing -> do
              e' <- lookupName i b
              pure e'
            Just {} -> pure x
        LiteralExpression {} -> return x
        ConstructorExpression {} -> return x
        ApplicationExpression l func arg ->
          case func of
            LambdaExpression l0 (Alternative l' params body) ->
              case params of
                (VariablePattern _ param:params') ->
                  let body' = substitute param arg body
                  in case params' of
                       [] -> pure body'
                       _ ->
                         pure
                           (LambdaExpression l0 (Alternative l' params' body'))
                [] -> error "Unsupported lambda."
            ConstructorExpression{} -> do
              arg' <- expand specialSigs signatures arg b
              pure (ApplicationExpression l func arg')
            _ -> do
              func' <- expand specialSigs signatures func b
              pure (ApplicationExpression l func' arg)
        IfExpression l pr th el ->
          case pr of
            ConstructorExpression _ n
              | n == specialSigsTrue specialSigs -> pure th
              | n == specialSigsFalse specialSigs -> pure el
            _ -> IfExpression l <$> go pr <*> pure th <*> pure el
        InfixExpression {} -> return x
        LetExpression {} -> return x
        LambdaExpression {} -> return x
        CaseExpression {} -> return x

specialVar :: Eq i => SpecialSigs i -> i -> Bool
specialVar specialSigs i = i `elem` [specialSigsFalse specialSigs, specialSigsTrue specialSigs]

substitute :: Name -> Expression Name l -> Expression Name l -> Expression Name l
substitute i arg =
  \case
    VariableExpression l i'
      | i == i' -> arg
      | otherwise -> VariableExpression l i'
    x@ConstructorExpression {} -> x
    ApplicationExpression l f x ->
      ApplicationExpression l (substitute i arg f) (substitute i arg x)
    InfixExpression l x f y ->
      InfixExpression l (substitute i arg x) f (substitute i arg y)
    LetExpression {} -> error "let expressions unsupported."
    CaseExpression l e cases ->
      CaseExpression
        l
        (substitute i arg e)
        (map (\(pat, e') -> (pat, substitute i arg e')) cases)
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
              ImplicitlyTypedBinding _ (ValueName _ i) [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is))
