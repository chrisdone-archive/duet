-----------------------------------------------------------------------------
-- StaticMonad:		Static environment for Monad
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

module StaticMonad(module StaticPrelude,
                   module StaticMonad) where
import Static
import StaticPrelude

-----------------------------------------------------------------------------
-- Monad classes:

monadClasses =   addClass "MonadPlus" asig [IsIn cMonad [atype]]
             <:> instances instsMonadPlus

instsMonadPlus
 = [mkInst [] ([] :=> isIn1 cMonadPlus tMaybe),
    mkInst [] ([] :=> isIn1 cMonadPlus tList)]

cMonadPlus = "MonadPlus"
mzeroMfun  = "mzero" :>: (Forall [Kfun Star Star, Star]
                          ([isIn1 cMonadPlus (TGen 0)] :=> 
                           (TAp (TGen 0) (TGen 1))))
mplusMfun  = "mplus" :>: (Forall [Kfun Star Star, Star]
                          ([isIn1 cMonadPlus (TGen 0)] :=> 
                           (TAp (TGen 0) (TGen 1) `fn`
                            TAp (TGen 0) (TGen 1) `fn`
                            TAp (TGen 0) (TGen 1))))


-----------------------------------------------------------------------------
