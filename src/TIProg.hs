-----------------------------------------------------------------------------
-- TIProg:	Type Inference for Whole Programs
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

module TIProg where
import Subst
import Assump
import TIMonad
import Infer
import TIMain
import Pred

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $
                      do (ps, as') <- tiSeq tiBindGroup ce as bgs
                         s         <- getSubst
                         let rs     = reduce ce (apply s ps)
                         s'        <- defaultSubst ce [] rs
                         return (apply (s'@@s) as')

tiBindGroup' ce as bs = do (ps,as') <- tiBindGroup ce as bs
                           trim (tv (as'++as))
                           return (ps,as')

tiProgram' :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram' ce as bgs = runTI $
  do (ps, as') <- tiSeq tiBindGroup' ce as bgs
     s         <- getSubst
     let rs     = reduce ce (apply s ps)
     s'        <- defaultSubst ce [] rs
     return (apply (s'@@s) as')

-----------------------------------------------------------------------------
