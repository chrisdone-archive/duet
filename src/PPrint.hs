-----------------------------------------------------------------------------
-- PPrint:	Print functions
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

module PPrint(module PPrint, module Text.PrettyPrint.HughesPJ) where

import Text.PrettyPrint.HughesPJ


-----------------------------------------------------------------------------
-- This module contains definitions that do not appear in the
-- typeset version of the paper.

-----------------------------------------------------------------------------
-- Pretty printing; a replacement for Show:

pretty  :: PPrint a => a -> String
pretty   = render . pprint

ppParen    :: Bool -> Doc -> Doc
ppParen t x = if t then parens x else x

class PPrint a where
  pprint    :: a -> Doc

  parPprint :: a -> Doc
  parPprint  = parens . pprint

  pplist    :: [a] -> Doc
  pplist    xs = brackets (fsep (punctuate comma (map pprint xs)))

instance PPrint a => PPrint [a] where
  pprint  = pplist

instance PPrint Char where
  pprint  = char
  pplist  = text

instance PPrint Integer where
  pprint  = integer

instance PPrint Int where
  pprint  = int

instance PPrint Float where
  pprint  = float

instance PPrint Double where
  pprint  = double

instance (PPrint a, PPrint b) => PPrint (a,b) where
  pprint (x,y) = parens (sep [pprint x <> comma, pprint y])

instance (PPrint a, PPrint b, PPrint c) => PPrint (a,b,c) where
  pprint (x,y,z) = parens (sep [pprint x <> comma,
                                pprint y <> comma,
                                pprint z])

-----------------------------------------------------------------------------
