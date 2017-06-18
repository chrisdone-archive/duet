{-# LANGUAGE FlexibleContexts #-}

-- | Functions for setting up the context.

module Duet.Context where

import           Control.Monad.Catch
import           Control.Monad.Supply
import qualified Data.Map.Strict as M
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
