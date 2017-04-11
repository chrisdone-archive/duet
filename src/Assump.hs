-----------------------------------------------------------------------------
-- Assump:	Assumptions
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

module Assump where
import Id
import Scheme
import Subst
import PPrint

data Assump = Id :>: Scheme

instance PPrint Assump where
  pprint (i :>: s) = (text (show i) <+> text ":>:") $$ nest 2 (pprint s)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc)      = tv sc

find                 :: Monad m => Id -> [Assump] -> m Scheme
find i []             = fail ("unbound identifier: " ++ i)
find i ((i':>:sc):as) = if i==i' then return sc else find i as

-----------------------------------------------------------------------------
