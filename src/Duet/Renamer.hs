{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- At each binding point (lambdas), we need to supply a new unique
-- name, and then rename everything inside the expression.
--
-- For each BindGroup, we should generate the list of unique names
-- first for each top-level thing (which might be mutually
-- independent), and then run the sub-renaming processes, with the new
-- substitutions in scope.
--
-- It's as simple as that.

module Duet.Renamer where

import           Control.Arrow
import           Control.Monad.Catch
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Duet.Types

--------------------------------------------------------------------------------
-- Data type renaming

renameDataTypes
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> [DataType FieldType Identifier]
  -> m [DataType Type Name]
renameDataTypes subs types = do
  nameidents <-
    mapM
      (\(DataType name vars _) ->
         fmap (name, length vars, ) (supplyTypeName name))
      types
  let subs' = (<> subs) (M.fromList (map (\(x, _, z) -> (x, z)) nameidents))
      arities = map (\(_,arity,name) -> (name,arity)) nameidents
  mapM (renameDataType subs' arities) types

renameDataType
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> [(Name, Int)]
  -> DataType FieldType Identifier
  -> m (DataType Type Name)
renameDataType subs arities (DataType name vars cons) = do
  name' <- substitute subs name
  subs' <-
    fmap
      ((<> subs) . M.fromList)
      (mapM (\v -> fmap (v, ) (supplyTypeName v)) vars)
  vars' <- mapM (substitute subs') vars
  cons' <- mapM (renameConstructor arities subs') cons
  return (DataType name' vars' cons')

renameConstructor
  :: (MonadSupply Int m, MonadThrow m)
  => [(Name, Int)]
  -> Map Identifier Name
  -> DataTypeConstructor FieldType Identifier
  -> m (DataTypeConstructor Type Name)
renameConstructor arities subs (DataTypeConstructor name fields) =
  do name' <- supplyValueName name
     fields' <- mapM (renameFieldType arities subs) fields
     pure (DataTypeConstructor name' [])

--
-- We need kind inference here:
--
-- https://www.haskell.org/onlinereport/decls.html#kindinference
--
-- The report is mildly helpful, in that if a kind is unclear, then
-- you just default to *.
--
-- The `m` below is the only thing used in type-function position, so
-- that's a * -> *. The rest is by default `*`. In the case of `P`, we
-- don't know that `m` is *->*, so we should do unification somewhere.
--
-- data StateT s m a = StateT (s -> m a)
-- data P x m a = P (StateT x m a)
--

-- WORKAROUND:
--
-- Demand explicit kind signatures: all kinds are *, unless a
-- signature e.g. (m :: * -> *) is provided.
--
-- data StateT s (m :: * -> *) a = StateT (s -> m a)
-- data P x m a = P (StateT x m a)
--
-- Checker can demand that (m::*) and (m::*->*) match, requiring:

-- data P x (m :: * -> *) a = ..


renameFieldType
  :: MonadThrow m
  => [(Name,Int)] -> Map Identifier Name -> FieldType Identifier -> m (Type Name)
renameFieldType arities subs = go
  where
    go  =
      \case
        FieldTypeConstructor i -> do
          name <- substitute subs i
          kind <- kindOf arities name
          pure (ConstructorType (TypeConstructor name kind))
        FieldTypeVariable i -> do
          name <- substitute subs i
          pure (VariableType (TypeVariable name StarKind))
        FieldTypeApp f x -> do
          f' <- go f
          x' <- go x
          pure (ApplicationType f' x')


boolDataType :: DataType FieldType Identifier
boolDataType =
  DataType
    (Identifier "Bool")
    []
    [ DataTypeConstructor (Identifier "True") []
    , DataTypeConstructor (Identifier "False") []
    ]

maybeDataType :: DataType FieldType Identifier
maybeDataType =
  DataType
    (Identifier "Maybe")
    [Identifier "a"]
    [ DataTypeConstructor (Identifier "Nothing") []
    , DataTypeConstructor
        (Identifier "Just")
        [FieldTypeVariable (Identifier "a")]
    ]

eitherDataType :: DataType FieldType Identifier
eitherDataType =
  DataType
    (Identifier "Either")
    [Identifier "a", Identifier "b"]
    [ DataTypeConstructor
        (Identifier "Left")
        [FieldTypeVariable (Identifier "a")]
    , DataTypeConstructor
        (Identifier "Right")
        [FieldTypeVariable (Identifier "b")]
    ]

--------------------------------------------------------------------------------
-- Value renaming

renameBindGroups
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> [BindGroup Identifier l]
  -> m ([BindGroup Name l], Map Identifier Name)
renameBindGroups subs groups = do
  subs' <-
    fmap
      mconcat
      (mapM (\(BindGroup ex implicit) -> getImplicitSubs subs implicit) groups) -- TODO: explicit
  fmap (second mconcat . unzip) (mapM (renameBindGroup subs') groups)

renameBindGroup
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> BindGroup Identifier l
  -> m (BindGroup Name l, Map Identifier Name)
renameBindGroup subs (BindGroup explicit implicit) = do
  bindGroup' <-
    BindGroup <$> renameExplicit subs explicit <*>
    mapM (mapM (renameImplicit subs)) implicit
  pure (bindGroup', subs)

getImplicitSubs
  :: (MonadSupply Int m)
  => Map Identifier Name
  -> [[ImplicitlyTypedBinding Identifier l]]
  -> m (Map Identifier Name)
getImplicitSubs subs implicit =
  fmap
    ((<> subs) . M.fromList)
    (mapM
       (\(ImplicitlyTypedBinding _ i _) -> fmap (i, ) (supplyValueName i))
       (concat implicit))

renameExplicit :: Applicative f => t2 -> t1 -> f [t]
renameExplicit _ _ = pure [] -- TODO:

renameImplicit
  :: (MonadThrow m,MonadSupply Int m)
  => Map Identifier Name
  -> ImplicitlyTypedBinding Identifier t
  -> m (ImplicitlyTypedBinding Name t)
renameImplicit subs (ImplicitlyTypedBinding l id' alts) =
  do name <- substitute subs id'
     ImplicitlyTypedBinding l name <$> mapM (renameAlt subs) alts

renameAlt
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name -> Alternative Identifier l -> m (Alternative Name l)
renameAlt subs (Alternative l ps e) =
  do (ps', subs') <- runWriterT (mapM renamePattern ps)
     let subs'' = M.fromList subs' <> subs
     Alternative l <$> pure ps' <*> renameExpression subs'' e

renamePattern
  :: MonadSupply Int m
  => Pattern Identifier
  -> WriterT [(Identifier, Name)] m (Pattern Name)
renamePattern =
  \case
    VariablePattern i -> do
      name <- lift (supplyValueName i)
      tell [(i, name)]
      pure (VariablePattern name)
    WildcardPattern -> pure WildcardPattern
    AsPattern i p -> do
      name <- supplyValueName i
      tell [(i, name)]
      AsPattern name <$> renamePattern p
    LiteralPattern l -> pure (LiteralPattern l)
    ConstructorPattern {} -> error "TODO: ConstructorPattern"

renameExpression
  :: (MonadThrow m, MonadSupply Int m)
  => Map Identifier Name -> Expression Identifier l -> m (Expression Name l)
renameExpression subs = go
  where
    go =
      \case
        VariableExpression l i -> VariableExpression l <$> substitute subs i
        LiteralExpression l i -> pure (LiteralExpression l i)
        ConstantExpression {} -> error "TODO: ConstantExpression"
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        InfixExpression l x i y ->
          InfixExpression l <$> go x <*> substitute subs i <*> go y
        LetExpression l bindGroup@(BindGroup ex implicit) e -> do
          subs' <- getImplicitSubs subs implicit
          (bindGroup', subs'') <- renameBindGroup subs' bindGroup
          LetExpression l <$> pure bindGroup' <*> renameExpression subs'' e
        LambdaExpression l alt -> LambdaExpression l <$> renameAlt subs alt
        IfExpression l x y z -> IfExpression l <$> go x <*> go y <*> go z
        CaseExpression l e pat_exps ->
          CaseExpression l <$> go e <*>
          mapM
            (\(pat, ex) -> do
               (pat', subs') <- runWriterT (renamePattern pat)
               e' <- renameExpression (M.fromList subs' <> subs) ex
               pure (pat', e'))
            pat_exps

--------------------------------------------------------------------------------
-- Generate a kind for a data type

kindOf :: MonadThrow m => [(Name, Int)] -> Name -> m Kind
kindOf arities name =
  case lookup name arities of
    Nothing -> throwM (TypeNotInScope (map fst arities) name)
    Just arity -> pure (foldr FunctionKind StarKind (replicate arity StarKind))

--------------------------------------------------------------------------------
-- Provide a substitution

substitute :: MonadThrow m => Map Identifier Name -> Identifier -> m Name
substitute subs i =
  case M.lookup i subs of
    Nothing -> throwM (IdentifierNotInScope subs i)
    Just name -> pure name

--------------------------------------------------------------------------------
-- Provide a new name

supplyValueName :: (MonadSupply Int m) => Identifier -> m Name
supplyValueName (Identifier s) = do
  i <- supply
  return (ValueName i s)

supplyTypeName :: (MonadSupply Int m) => Identifier -> m Name
supplyTypeName (Identifier s) = do
  i <- supply
  return (TypeName i s)
