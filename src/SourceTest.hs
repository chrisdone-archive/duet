-----------------------------------------------------------------------------
-- SourceTest:		Haskell encoding of a Test file source code
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

module SourceTest where
import Testbed
import StaticTest
import HaskellPrelude

-----------------------------------------------------------------------------
-- Test Framework:

main       :: IO ()
main        = test static imports testDefns

saveTest   :: IO ()
saveTest    = save "Test" static imports testDefns

Just static = (preludeClasses <:> testClasses) initialEnv

imports    :: [Assump]
imports     = defnsHaskellPrelude

-----------------------------------------------------------------------------
-- Test Programs:

testDefns :: [BindGroup]
testDefns
 = map toBg 
   [
--  test x         = x==x

    [("test",
      Nothing,
      [([PVar "x"],
        ap [econst eqMfun, evar "x", evar "x"])])],

--  head' (x:xs)   = x    where dummy = tail'
--  tail' (x:xs)   = xs   where dummy = head'

    [("head'",
      Nothing,
      [([PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("dummy",
                Nothing,
                [([],
                  evar "tail'")])]]
             (evar "x"))]),
     ("tail'",
      Nothing,
      [([PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("dummy",
                Nothing,
                [([],
                  evar "head'")])]]
             (evar "xs"))])],

--  aFloat        :: Float
--  aFloat         = 23

    [("aFloat",
      Just (Forall []
             ([] :=> 
              tFloat)),
      [([],
        elit (LitInt 23))])],

--  NB. Hugs defaults this to an integer ... it shouldn't
--  aNum           = 42

    [("aNum",
      Nothing,
      [([],
        elit (LitInt 42))])],

--  NB. Hugs reports a type error here because of the premature defaulting
--  of aNum ...
--  bNum           = aNum + aFloat

    [("bNum",
      Nothing,
      [([],
        ap [econst plusMfun, evar "aNum", evar "aFloat"])])],

--  cNum x         = subtract aFloat x

    [("cNum",
      Nothing,
      [([PVar "x"],
        ap [evar "subtract", evar "aFloat", evar "x"])])],

--  This example falls foul of monomorphism:
--  equal       = (==)
{-
    [("equal",
      Nothing,
      [([],
        econst eqMfun)])],
-}

--  oddMono        = not . even

    [("oddMono",
      Nothing,
      [([],
        ap [evar ".", evar "not", econst evenMfun])])],

--  sumMono        = foldl (+) 0

    [("sumMono",
      Nothing,
      [([],
        ap [evar "foldl", econst plusMfun, elit (LitInt 0)])])],

--  sumEta xs      = foldl (+) 0 xs

    [("sumEta",
      Nothing,
      [([PVar "xs"],
        ap [evar "foldl", econst plusMfun, elit (LitInt 0), evar "xs"])])],

--  egf           :: Eq a => a -> Bool
--  egf x          = x==x || egg True
--  egg           :: Ord a => a -> Bool
--  egg x          = x<=x || egg True

    [("egg",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=> 
              (TGen 0 `fn` tBool))),
      [([PVar "x"],
        ap [evar "||", ap [econst leMfun, evar "x", evar "x"],
                       ap [evar "egg", econst trueCfun]])])],
    [("egf",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TGen 0 `fn` tBool))),
      [([PVar "x"],
        ap [evar "||", ap [econst eqMfun, evar "x", evar "x"],
                       ap [evar "egg", econst trueCfun]])])],

--  egf'          :: Eq a => a -> Bool
--  egf' x         = x==x || egg' True
--  egg' x         = x<=x || egf' True

    [("egf'",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TGen 0 `fn` tBool))),
      [([PVar "x"],
        ap [evar "||", ap [econst eqMfun, evar "x", evar "x"],
                       ap [evar "egg'", econst trueCfun]])]),
     ("egg'",
      Nothing,
      [([PVar "x"],
        ap [evar "||", ap [econst leMfun, evar "x", evar "x"],
                       ap [evar "egf'", econst trueCfun]])])],

--  egf'' x        = x==x || egg'' True
--  egg'' x        = x<=x || egf'' True

    [("egf''",
      Nothing,
      [([PVar "x"],
        ap [evar "||", ap [econst eqMfun, evar "x", evar "x"],
                       ap [evar "egg''", econst trueCfun]])]),
     ("egg''",
      Nothing,
      [([PVar "x"],
        ap [evar "||", ap [econst leMfun, evar "x", evar "x"],
                       ap [evar "egf''", econst trueCfun]])])],

--  myOdd 0        = False
--  myOdd n        = not (myEven (n-1))
--  myEven 0       = True
--  myEven n       = not (myOdd (n-1))

    [("myOdd",
      Nothing,
      [([PLit (LitInt 0)],
        econst falseCfun),
       ([PVar "n"],
        ap [evar "not", ap [evar "myEven",
                          ap [econst minusMfun, evar "n", elit (LitInt 1)]]])]),
     ("myEven",
      Nothing,
      [([PLit (LitInt 0)],
        econst trueCfun),
       ([PVar "n"],
        ap [evar "not",ap [evar "myOdd",
                         ap [econst minusMfun, evar "n", elit (LitInt 1)]]])])],

--  This example demonstrates a problem with ambiguities ...
--  ambig x | c==c = c >> return x
--         where c = return 0
{-

    [("ambig",
      Nothing,
      [([PVar "x"],
        elet [[("c",
                Nothing,
                [([], ap [econst returnMfun, (elit (LitInt 0)) ])])]]
             (eguarded [(ap [econst eqMfun, evar "c", evar "c"],
                         ap [econst mthenMfun, evar "c",
                           ap [econst returnMfun, evar "x"]])]))])],
-}

--  eqNull        :: Eq a => [a] -> Bool
--  eqNull xs      = xs==[]

    [("eqNull",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` tBool))),
      [([PVar "xs"],
        ap [econst eqMfun, evar "xs", econst nilCfun])])],

--  eqNull' xs     = xs==[]

    [("eqNull'",
      Nothing,
      [([PVar "xs"],
        ap [econst eqMfun, evar "xs", econst nilCfun])])],

--  baz x          = (((x==), (x<=)), ((rem x, (x+))))

    [("baz",
      Nothing,
      [([PVar "x"],
        ap [econst tup2Cfun,
            ap [econst tup2Cfun,
                ap [econst eqMfun, evar "x"],
                ap [econst leMfun, evar "x"]],
            ap [econst tup2Cfun,
                ap [econst remMfun, evar "x"],
                ap [econst plusMfun, evar "x"]]])])]]

-----------------------------------------------------------------------------
