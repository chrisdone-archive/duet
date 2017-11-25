{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | The substitution stepper.

module Duet.Stepper
  ( expandSeq1
  , fargs
  , lookupNameByString
  ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.State
import           Control.Monad.Supply
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Semigroup
import           Duet.Types

--------------------------------------------------------------------------------
-- Expansion

expandSeq1
  :: (MonadThrow m, MonadSupply Int m)
  => Context Type Name (Location)
  -> [BindGroup Type Name (TypeSignature Type Name Duet.Types.Location)]
  -> Expression Type Name (TypeSignature Type Name Location)
  -> m (Expression Type Name (TypeSignature Type Name Location))
expandSeq1 (Context { contextTypeClasses = typeClassEnv
                    , contextSpecialSigs = specialSigs
                    , contextSignatures = signatures
                    }) b e = evalStateT (go e) False
  where
    go =
      \case
        e0
          | (ce@(ConstructorExpression l _), args) <- fargs e0 -> do
            args' <- mapM go args
            pure (foldl (ApplicationExpression l) ce args')
          | (ce@(ConstantExpression l _), args) <- fargs e0 -> do
            args' <- mapM go args
            pure (foldl (ApplicationExpression l) ce args')
          | otherwise -> do
            alreadyExpanded <- get
            if alreadyExpanded
              then pure e0
              else do
                e' <- lift (expandWhnf typeClassEnv specialSigs signatures e0 b)
                put (e' /= e0)
                pure e'

expandWhnf
  :: MonadThrow m
  => Map Name (Class Type Name (TypeSignature Type Name Location))
  -> SpecialSigs Name
  -> [TypeSignature Type Name Name]
  -> Expression Type Name (TypeSignature Type Name Location)
  -> [BindGroup Type Name (TypeSignature Type Name Duet.Types.Location)]
  -> m (Expression Type Name (TypeSignature Type Name Location))
expandWhnf typeClassEnv specialSigs signatures e b = go e
  where
    go x =
      case x of
        VariableExpression _ i -> do
          case find ((== i) . typeSignatureA) signatures of
            Nothing -> do
              e' <- lookupName i b
              pure e'
            Just {} -> pure x
        LiteralExpression {} -> return x
        ConstructorExpression {} -> return x
        ConstantExpression {} -> return x
        ApplicationExpression l (ApplicationExpression l1 op@(VariableExpression _ (PrimopName primop)) x) y ->
          case x of
            LiteralExpression _ (StringLiteral sx) ->
              case y of
                LiteralExpression _ (StringLiteral sy) ->
                  case primop of
                    PrimopStringAppend ->
                      pure (LiteralExpression l (StringLiteral (sx <> sy)))
                _ -> do
                  y' <- go y
                  pure
                    (ApplicationExpression l (ApplicationExpression l1 op x) y')
            _ -> do
              x' <- go x
              pure (ApplicationExpression l (ApplicationExpression l1 op x') y)
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
            VariableExpression _ (MethodName _ methodName) ->
              case arg of
                VariableExpression _ dictName@DictName {} ->
                  case find
                         ((== dictName) . dictionaryName)
                         (concatMap
                            (map instanceDictionary . classInstances)
                            (M.elems typeClassEnv)) of
                    Nothing -> throwM (CouldntFindMethodDict dictName)
                    Just dict ->
                      case M.lookup
                             methodName
                             (M.mapKeys
                                (\(MethodName _ s) -> s)
                                (dictionaryMethods dict)) of
                        Nothing ->
                          error
                            ("Missing method " ++
                             show methodName ++ " in dictionary: " ++ show dict)
                        Just (Alternative _ _ e) -> pure e
            _ -> do
              func' <- go func
              pure (ApplicationExpression l func' arg)
        orig@(InfixExpression l x op@(s, VariableExpression _ (PrimopName primop)) y) ->
          case x of
            LiteralExpression _ x' ->
              case y of
                LiteralExpression _ y' ->
                  case (x', y') of
                    (IntegerLiteral i1, IntegerLiteral i2) ->
                      pure
                        (LiteralExpression
                           l
                           (case primop of
                              PrimopIntegerPlus -> IntegerLiteral (i1 + i2)
                              PrimopIntegerTimes -> IntegerLiteral (i1 * i2)
                              PrimopIntegerSubtract -> IntegerLiteral (i1 - i2)))
                    (RationalLiteral i1, RationalLiteral i2) ->
                      pure
                        (LiteralExpression
                           l
                           (case primop of
                              PrimopRationalPlus -> RationalLiteral (i1 + i2)
                              PrimopRationalTimes -> RationalLiteral (i1 * i2)
                              PrimopRationalSubtract ->
                                RationalLiteral (i1 - i2)
                              PrimopRationalDivide -> RationalLiteral (i1 / i2)))
                    _ -> pure orig
                _ -> do
                  y' <- go y
                  pure (InfixExpression l x op y')
            _ -> do
              x' <- go x
              pure (InfixExpression l x' op y)
        InfixExpression l x (s, op) y -> do
          op' <- go op
          pure (InfixExpression l x (s, op') y)
        IfExpression l pr th el ->
          case pr of
            ConstructorExpression _ n
              | n == specialSigsTrue specialSigs -> pure th
              | n == specialSigsFalse specialSigs -> pure el
            _ -> IfExpression l <$> go pr <*> pure th <*> pure el
        LetExpression {} -> return x
        LambdaExpression {} -> return x
        CaseExpression l e0 alts ->
          let matches =
                map
                  (\ca -> (match e0 (caseAltPattern ca), caseAltExpression ca))
                  alts
          in case listToMaybe
                    (mapMaybe
                       (\(r, e) -> do
                          case r of
                            OK v -> pure (v, e)
                            Fail -> Nothing)
                       matches) of
               Just (Success subs, expr) ->
                 return
                   (foldr
                      (\(name, that) expr' -> substitute name that expr')
                      expr
                      subs)
               Just (NeedsMoreEval is, _) -> do
                 e' <- expandAt typeClassEnv is specialSigs signatures e0 b
                 pure (CaseExpression l e' alts)
               Nothing -> error ("Incomplete pattern match... " ++ show matches)

expandAt
  :: MonadThrow m
  => Map Name (Class Type Name (TypeSignature Type Name Location))
  -> [Int]
  -> SpecialSigs Name
  -> [TypeSignature Type Name Name]
  -> Expression Type Name (TypeSignature Type Name Location)
  -> [BindGroup Type Name (TypeSignature Type Name Duet.Types.Location)]
  -> m (Expression Type Name (TypeSignature Type Name Location))
expandAt typeClassEnv is specialSigs signatures e0 b  = go [0] e0
  where
    go js e =
      if is == js
        then expandWhnf typeClassEnv specialSigs signatures e b
        else case e of
               _
                 | (ce@(ConstructorExpression l _), args) <- fargs e -> do
                   args' <-
                     sequence
                       (zipWith (\i arg -> go (js ++ [i]) arg) [0 ..] args)
                   pure (foldl (ApplicationExpression l) ce args')
                 | otherwise -> pure e

--------------------------------------------------------------------------------
-- Pattern matching

match
  :: (Show l, Show i, Eq i)
  => Expression Type i l -> Pattern Type i l -> Result (Match Type i l)
match = go [0]
  where
    go is val pat =
      case pat of
        WildcardPattern _ _ -> OK (Success [])
        VariablePattern _ i -> OK (Success [(i, val)])
        LiteralPattern _ l ->
          case val of
            LiteralExpression _ l'
              | l' == l -> OK (Success [])
              | otherwise -> Fail
            _ -> OK (NeedsMoreEval is)
        ConstructorPattern _ i pats
          | (constructor@ConstructorExpression {}, args) <- fargs val ->
            if fmap (const ()) constructor == ConstructorExpression () i
              then if length args == length pats
                     then foldl
                            (<>)
                            (OK (Success []))
                            (zipWith
                               (\j (arg, p) -> go (is ++ [j]) arg p)
                               [0 ..]
                               (zip args pats))
                     else Fail
              else Fail
          | otherwise -> OK (NeedsMoreEval is)

--------------------------------------------------------------------------------
-- Expression manipulators

-- | Flatten an application f x y into (f,[x,y]).
fargs :: Expression Type i l -> (Expression Type i l, [(Expression Type i l)])
fargs e = go e []
  where
    go (ApplicationExpression _ f x) args = go f (x : args)
    go f args = (f, args)

--------------------------------------------------------------------------------
-- Substitutions

substitute :: Eq i => i -> Expression Type i l -> Expression Type i l -> Expression Type i l
substitute i arg = go
  where
    go =
      \case
        VariableExpression l i'
          | i == i' -> arg
          | otherwise -> VariableExpression l i'
        x@ConstructorExpression {} -> x
        x@ConstantExpression {} -> x
        ApplicationExpression l f x -> ApplicationExpression l (go f) (go x)
        InfixExpression l x (s, f) y -> InfixExpression l (go x) (s, go f) (go y)
        LetExpression {} -> error "let expressions unsupported."
        CaseExpression l e cases ->
          CaseExpression l (go e) (map (\(CaseAlt l pat e') -> CaseAlt l pat (go e')) cases)
        IfExpression l a b c -> IfExpression l (go a) (go b) (go c)
        x@LiteralExpression {} -> x
        LambdaExpression l (Alternative l' args body) ->
          LambdaExpression l (Alternative l' args (go body))

--------------------------------------------------------------------------------
-- Lookups

lookupName
  :: (MonadThrow m)
  => Name
  -> [BindGroup Type Name (TypeSignature Type Name Duet.Types.Location)]
  -> m (Expression Type Name (TypeSignature Type Name Location))
lookupName identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindName identifier)
    Just i -> pure i
  where
    findIdent (BindGroup es is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ (i, _) [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is)) <|>
      listToMaybe
        (mapMaybe
           (\case
              ExplicitlyTypedBinding i _ [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           es)

lookupNameByString
  :: (MonadThrow m)
  => String
  -> [BindGroup Type Name (TypeSignature Type Name Duet.Types.Location)]
  -> m (Expression Type Name (TypeSignature Type Name Location))
lookupNameByString identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindNameByString identifier)
    Just i -> pure i
  where
    findIdent (BindGroup es is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ (ValueName _ i, _) [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is)) <|>
      listToMaybe
        (mapMaybe
           (\case
              ExplicitlyTypedBinding (ValueName _ i) _ [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           es)
