-----------------------------------------------------------------------------
-- Kind:		Kinds
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

module Kind where
import PPrint

data Kind  = Star | Kfun Kind Kind
             deriving Eq

instance PPrint Kind where
  pprint    = ppkind 0
  parPprint = ppkind 10

ppkind             :: Int -> Kind -> Doc
ppkind d Star       = text "Star"
ppkind d (Kfun l r) = ppParen (d>=10)
                         (text "Kfun" <+> ppkind 10 l <+> ppkind 0 r)

-----------------------------------------------------------------------------
