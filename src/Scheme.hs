-----------------------------------------------------------------------------
-- Scheme:	Type schemes
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------

module Scheme where
import Kind
import Type
import Subst
import Pred
import PPrint

data Scheme = Forall [Kind] (Qual Type)
              deriving Eq

instance PPrint Scheme where
  pprint (Forall ks qt)
    = (text "Forall" <+> pprint ks) $$ nest 2 (parPprint qt)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall ks qt)      = tv qt

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
 where vs' = [ v | v <- tv qt, v `elem` vs ]
       ks  = map kind vs'
       s   = zip vs' (map TGen [0..])

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

-----------------------------------------------------------------------------
