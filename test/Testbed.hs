-----------------------------------------------------------------------------
-- Testbed:             Utility functions for constructing test samples
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

module Testbed(module THIH,
               test, save) where
import THIH
import Text.PrettyPrint.HughesPJ

test          :: ClassEnv -> [Assumption] -> [BindGroup] -> IO ()
test ce as bgs = putStr $ render $ vcat $ map pprint $ reverse $
                 tiProgram' ce as bgs

save          :: String -> ClassEnv -> [Assumption] -> [BindGroup] -> IO ()
save f ce as bgs
               = writeFile ("Haskell" ++ f ++".hs")
                 ("-- Automatically generated typing assumptions for " ++ f ++
                 "\n\nmodule Haskell" ++ f ++ " where\n\
                  \import Testbed\n\
                  \import Static" ++ f ++ "\n\n\
                  \defnsHaskell" ++ f ++ "\n" ++
                  render
                   (text " = " <+>
                     brackets (fsep (punctuate comma (map pprint as'))))
                                        ++ "\n")
                 where as' = reverse $ tiProgram' ce as bgs

-----------------------------------------------------------------------------
