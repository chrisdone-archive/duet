-----------------------------------------------------------------------------
-- Static:		Utility functions for building static environments
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

module Static(module Id, module Kind, module Type, module Subst,
              module Pred, module Scheme, module Assump, module TIMonad,
              isIn1, mkInst, instances) where
import Id
import Kind
import Type
import Subst
import Pred
import Scheme
import Assump
import TIMonad

-----------------------------------------------------------------------------
-- This module contains definitions that do not appear in the
-- typeset version of the paper.

-- We introduce the following definition because it will allow a smoother
-- transition to multiple parameter classes later on ...

isIn1       :: Id -> Type -> Pred
isIn1 i t    = IsIn i [t]

mkInst      :: Instantiate a => [Kind] -> a -> a
mkInst ks = inst ts 
 where ts   = zipWith (\v k -> TVar (Tyvar v k)) vars ks
       vars = [ [c] | c <-['a'..'z'] ] ++
              [ c : show n | n <-[0::Int ..], c<-['a'..'z'] ]

instances  :: [Inst] -> EnvTransformer
instances   = foldr1 (<:>) . map (\(ps:=>p) -> addInst ps p)

-----------------------------------------------------------------------------
