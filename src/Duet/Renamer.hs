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

import           Control.Monad.Supply
import           Control.Monad.Catch
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Duet.Types

--------------------------------------------------------------------------------
-- Perform renaming

renameBindGroup
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> BindGroup Identifier Location
  -> m (BindGroup Name Location)
renameBindGroup subs (BindGroup explicit implicit) =
  do let subs' = undefined subs implicit
     BindGroup <$> renameExplicit subs' explicit <*> mapM (mapM (renameImplicit subs')) implicit

renameExplicit _ _ = pure [] -- TODO:

renameImplicit subs (ImplicitlyTypedBinding l id alts) =
  do name <- substitute subs id
     ImplicitlyTypedBinding l name <$> mapM renameAlt alts

renameAlt (Alternative l ps e) =
  do undefined
     undefined

--------------------------------------------------------------------------------
-- Provide a substitution

substitute :: MonadThrow m => Map Identifier Name -> Identifier -> m Name
substitute = undefined

--------------------------------------------------------------------------------
-- Provide a new name

supplyName :: (Monad m) => Identifier -> SupplyT Int m Name
supplyName (Identifier s) = do
  i <- supply
  return (NameFromSource i s)
