{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for setting up the context.

module Duet.Context where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Supply
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Duet.Infer
import           Duet.Renamer
import           Duet.Supply
import           Duet.Types

-- | Make an instance.
makeInst
  :: MonadSupply Int m
  => Specials Name
  -> Predicate Type Name
  -> [(String, Alternative Type Name l)]
  -> m (Instance Type Name l)
makeInst specials pred' methods = do
  name <- supplyDictName (predicateToDict specials pred')
  methods' <-
    mapM
      (\(key, alt) -> do
         key' <- supplyMethodName (Identifier key)
         pure (key', alt))
      methods
  pure (Instance (Qualified [] pred') (Dictionary name (M.fromList methods')))

-- | Make a class.
makeClass
  :: MonadSupply Int m
  => Identifier
  -> [TypeVariable Name]
  -> [(Name, Scheme t Name)]
  -> m (Class t Name l)
makeClass name vars methods = do
  name' <- supplyClassName name
  pure
    (Class
     { className = name'
     , classTypeVariables = vars
     , classInstances = []
     , classMethods = M.fromList methods
     , classSuperclasses = mempty
     })

-- | Generate signatures from a data type.
dataTypeSignatures
  :: Monad m
  => SpecialTypes Name -> DataType Type Name -> m [TypeSignature Type Name Name]
dataTypeSignatures specialTypes dt@(DataType _ vs cs) = mapM construct cs
  where
    construct (DataTypeConstructor cname fs) =
      pure
        (TypeSignature
           cname
           (Forall
              vs
              (Qualified
                 []
                 (foldr
                    makeArrow
                    (foldl
                       ApplicationType
                       (dataTypeConstructor dt)
                       (map VariableType vs))
                    fs))))
      where
        makeArrow :: Type Name -> Type Name -> Type Name
        a `makeArrow` b =
          ApplicationType
            (ApplicationType
               (ConstructorType (specialTypesFunction specialTypes))
               a)
            b

-- | Make signatures from a class.
classSignatures
  :: MonadThrow m
  => Class Type Name l -> m [TypeSignature Type Name Name]
classSignatures cls =
  mapM
    (\(name, scheme) ->
       TypeSignature <$> pure name <*> classMethodScheme cls scheme)
    (M.toList (classMethods cls))

builtinsSpecials :: Builtins t i l -> Specials i
builtinsSpecials builtins =
  Specials (builtinsSpecialSigs builtins) (builtinsSpecialTypes builtins)

contextSpecials :: Context t i l -> Specials i
contextSpecials context =
  Specials (contextSpecialSigs context) (contextSpecialTypes context)

generateAllSignatures builtins dataTypes typeClasses =
  do consSigs <-
       fmap
         concat
         (mapM (dataTypeSignatures (builtinsSpecialTypes builtins)) dataTypes)
     methodSigs <- fmap concat (mapM classSignatures typeClasses)
     pure (builtinsSignatures builtins <> consSigs <> methodSigs)

makeScope typeClasses signatures =
  pure
    (M.fromList
       (mapMaybe
          (\(TypeSignature name _) ->
             case name of
               ValueName _ ident -> Just (Identifier ident, name)
               ConstructorName _ ident -> pure (Identifier ident, name)
               MethodName _ ident -> pure (Identifier ident, name)
               _ -> Nothing)
          signatures) <>
     M.map className typeClasses)

renameEverything decls specials builtins = do
  dataTypes <- renameDataTypes specials (declsDataTypes decls)
  (typeClasses, signatures, subs) <-
    do typeClasses <-
         fmap
           M.fromList
           (mapM
              (\c -> do
                 renamed <- renameClass specials mempty dataTypes c
                 pure (className c, renamed))
              classes)
       signatures <- generateAllSignatures builtins dataTypes typeClasses
       scope <- makeScope typeClasses signatures
       allInstances <-
         mapM
           (renameInstance specials scope dataTypes (M.elems typeClasses))
           instances
       pure
         ( M.map
             (\typeClass ->
                typeClass
                { classInstances =
                    filter
                      ((== className typeClass) . instanceClassName)
                      allInstances
                })
             typeClasses
         , signatures
         , scope)
  (renamedBindings, subs') <- renameBindGroups specials subs dataTypes bindings
  pure (typeClasses, signatures, renamedBindings, subs', dataTypes)
  where declsDataTypes =
          mapMaybe
            (\case
               DataDecl d -> Just d
               _ -> Nothing)
        bindings =
          mapMaybe
            (\case
               BindGroupDecl d -> Just d
               _ -> Nothing)
            decls
        classes =
          mapMaybe
            (\case
               ClassDecl d -> Just d
               _ -> Nothing)
            decls
        instances =
          mapMaybe
            (\case
               InstanceDecl d -> Just d
               _ -> Nothing)
            decls

addClasses builtins typeClasses =
  foldM
    (\e0 typeClass ->
       addClass typeClass e0 >>= \e ->
         foldM (\e1 i -> do addInstance i e1) e (classInstances typeClass))
    (builtinsTypeClasses builtins)
    typeClasses
