module Duet.Renamer where

import Duet.Types
import Control.Monad.Supply

supplyName :: Monad m => Identifier -> SupplyT Int m Name
supplyName (Identifier s) = do
  i <- supply
  return (NameFromSource i s)

rename :: a
rename = undefined
