{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Duet.Infer
import           Duet.Types

class Identifiable i where
  identifyValue :: MonadThrow m => i -> m Identifier

instance Identifiable Identifier where
  identifyValue = pure

instance Identifiable Name where
  identifyValue =
    \case
      ValueName _ i -> pure (Identifier i)
      ConstructorName _ c -> pure (Identifier c)
      n@TypeName {} -> throwM (TypeAtValueScope n)
      n@ForallName {} -> throwM (TypeAtValueScope n)

--------------------------------------------------------------------------------
-- Data type renaming

renameDataTypes
  :: (MonadSupply Int m, MonadThrow m)
  => SpecialTypes Name
  -> [DataType FieldType Identifier]
  -> m [DataType Type Name]
renameDataTypes specialTypes types = do
  typeConstructors <-
    mapM
      (\(DataType name vars cs) -> do
         name' <- supplyTypeName name
         vars' <-
           mapM
             (\(TypeVariable i k) -> do
                i' <- supplyTypeName i
                pure (i, TypeVariable i' k))
             vars
         pure (name, name', vars', cs))
      types
  mapM
    (\(_, name, vars, cs) -> do
       cs' <- mapM (renameConstructor specialTypes typeConstructors vars) cs
       pure (DataType name (map snd vars) cs'))
    typeConstructors

renameConstructor
  :: (MonadSupply Int m, MonadThrow m)
  => SpecialTypes Name -> [(Identifier, Name, [(Identifier, TypeVariable Name)], [DataTypeConstructor FieldType Identifier])]
  -> [(Identifier, TypeVariable Name)]
  -> DataTypeConstructor FieldType Identifier
  -> m (DataTypeConstructor Type Name)
renameConstructor specialTypes typeConstructors vars (DataTypeConstructor name fields) = do
  name' <- supplyConstructorName name
  fields' <- mapM (renameField specialTypes typeConstructors vars name') fields
  pure (DataTypeConstructor name' fields')

renameField
  :: MonadThrow m
  => SpecialTypes Name
  -> [(Identifier, Name, [(Identifier, TypeVariable Name)], [DataTypeConstructor FieldType Identifier])]
  -> [(Identifier, TypeVariable Name)]
  -> Name
  -> FieldType Identifier
  -> m (Type Name)
renameField specialTypes typeConstructors vars name fe = do
  ty <- go fe
  if typeKind ty == StarKind
    then pure ty
    else throwM (ConstructorFieldKind name ty (typeKind ty))
  where
    go =
      \case
        FieldTypeConstructor i -> do
          (name', vars') <- resolve i
          pure (ConstructorType (toTypeConstructor name' (map snd vars')))
        FieldTypeVariable v ->
          case lookup v vars of
            Nothing -> throwM (TypeNotInScope [] v)
            Just tyvar -> pure (VariableType tyvar)
        FieldTypeApp f x -> do
          f' <- go f
          let fKind = typeKind f'
          case fKind of
            FunctionKind argKind _ -> do
              x' <- go x
              let xKind = typeKind x'
              if xKind == argKind
                then pure (ApplicationType f' x')
                else throwM (RenamerKindMismatch f' fKind x' xKind)
            StarKind -> do
              x' <- go x
              throwM (KindTooManyArgs f' fKind x')
    resolve i =
      case find ((\(j, _, _, _) -> j == i)) typeConstructors of
        Just (_, name', vs, _) -> pure (name', vs)
        Nothing ->
          case specialTypesBool specialTypes of
            DataType n@(TypeName _ i') vars _
              | Identifier i' == i ->
                pure
                  ( n
                  , map
                      (\case
                         (TypeVariable n@(TypeName _ i) k) ->
                           (Identifier i, TypeVariable n k))
                      vars)

--------------------------------------------------------------------------------
-- Value renaming

renameBindGroups
  :: (MonadSupply Int m, MonadThrow m, Ord i, Identifiable i)
  => Map Identifier Name
  -> [BindGroup i l]
  -> m ([BindGroup Name l], Map Identifier Name)
renameBindGroups subs groups = do
  subs' <-
    fmap
      mconcat
      (mapM (\(BindGroup ex implicit) -> getImplicitSubs subs implicit) groups) -- TODO: explicit
  fmap (second mconcat . unzip) (mapM (renameBindGroup subs') groups)

renameBindGroup
  :: (MonadSupply Int m, MonadThrow m, Ord i, Identifiable i)
  => Map Identifier Name
  -> BindGroup i l
  -> m (BindGroup Name l, Map Identifier Name)
renameBindGroup subs (BindGroup explicit implicit) = do
  bindGroup' <-
    BindGroup <$> renameExplicit subs explicit <*>
    mapM (mapM (renameImplicit subs)) implicit
  pure (bindGroup', subs)

getImplicitSubs
  :: (MonadSupply Int m, Ord i, Identifiable i, MonadThrow m)
  => Map Identifier Name
  -> [[ImplicitlyTypedBinding i l]]
  -> m (Map Identifier Name)
getImplicitSubs subs implicit =
  fmap
    ((<> subs) . M.fromList)
    (mapM
       (\(ImplicitlyTypedBinding _ i _) -> do
          v <- identifyValue i
          fmap (v, ) (supplyValueName i))
       (concat implicit))

renameExplicit :: Applicative f => t2 -> t1 -> f [t]
renameExplicit _ _ = pure [] -- TODO:

renameImplicit
  :: (MonadThrow m,MonadSupply Int m,Ord i, Identifiable i)
  => Map Identifier Name
  -> ImplicitlyTypedBinding i t
  -> m (ImplicitlyTypedBinding Name t)
renameImplicit subs (ImplicitlyTypedBinding l id' alts) =
  do name <- substituteVar subs id'
     ImplicitlyTypedBinding l name <$> mapM (renameAlt subs) alts

renameAlt
  :: (MonadSupply Int m, MonadThrow m, Ord i , Ord i, Identifiable i)
  => Map Identifier Name -> Alternative i l -> m (Alternative Name l)
renameAlt subs (Alternative l ps e) =
  do (ps', subs') <- runWriterT (mapM (renamePattern subs) ps)
     let subs'' = M.fromList subs' <> subs
     Alternative l <$> pure ps' <*> renameExpression subs'' e

renamePattern
  :: (MonadSupply Int m, MonadThrow m, Ord i, Identifiable i)
  => Map Identifier Name
  -> Pattern i l
  -> WriterT [(Identifier, Name)] m (Pattern Name l)
renamePattern subs =
  \case
    VariablePattern l i -> do
      name <- lift (supplyValueName i)
      v <- identifyValue i
      tell [(v, name)]
      pure (VariablePattern l name)
    WildcardPattern l -> pure (WildcardPattern l)
    AsPattern l i p -> do
      name <- supplyValueName i
      v <- identifyValue i
      tell [(v, name)]
      AsPattern l name <$> renamePattern subs p
    LiteralPattern l0 l -> pure (LiteralPattern l0 l)
    ConstructorPattern l i pats ->
      ConstructorPattern l <$> substituteCons subs i <*>
      mapM (renamePattern subs) pats

renameExpression
  :: forall i m l. (MonadThrow m, MonadSupply Int m , Ord i, Identifiable i)
  => Map Identifier Name -> Expression i l -> m (Expression Name l)
renameExpression subs = go
  where
    go :: Expression i l -> m (Expression Name l)
    go =
      \case
        VariableExpression l i -> VariableExpression l <$> substituteVar subs i
        ConstructorExpression l i -> ConstructorExpression l <$> substituteCons subs i
        LiteralExpression l i -> pure (LiteralExpression l i)
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        InfixExpression l x i y ->
          InfixExpression l <$> go x <*> substituteVar subs i <*> go y
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
               (pat', subs') <- runWriterT (renamePattern subs pat)
               e' <- renameExpression (M.fromList subs' <> subs) ex
               pure (pat', e'))
            pat_exps

--------------------------------------------------------------------------------
-- Provide a substitution

substituteVar :: (Ord i, Identifiable i, MonadThrow m) => Map Identifier Name -> i -> m Name
substituteVar subs i0 =
  do i <- identifyValue i0
     case M.lookup i subs of
       Just name@ValueName{} -> pure name
       _ -> do s <- identifyValue i
               throwM (IdentifierNotInVarScope subs s)

substituteCons :: (Ord i, Identifiable i, MonadThrow m) => Map Identifier Name -> i -> m Name
substituteCons subs i0 =
  do i <- identifyValue i0
     case M.lookup i subs of
       Just name@ConstructorName{} -> pure name
       _ -> do  throwM (IdentifierNotInConScope subs i)

--------------------------------------------------------------------------------
-- Provide a new name

supplyValueName :: (MonadSupply Int m, Identifiable i, MonadThrow m) => i -> m Name
supplyValueName s = do
  i <- supply
  Identifier s <- identifyValue s
  return (ValueName i s)

supplyConstructorName :: (MonadSupply Int m) => Identifier -> m Name
supplyConstructorName (Identifier s) = do
  i <- supply
  return (ConstructorName i s)

supplyTypeName :: (MonadSupply Int m) => Identifier -> m Name
supplyTypeName (Identifier s) = do
  i <- supply
  return (TypeName i s)
