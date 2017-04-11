-----------------------------------------------------------------------------
-- SourceMaybe:		Haskell encoding of the Maybe library source code
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

module SourceMaybe where
import Testbed
import StaticMaybe
import HaskellPrims
import HaskellPrelude

-----------------------------------------------------------------------------
-- Test Framework:

main       :: IO ()
main        = test static imports maybeDefns

saveMaybe  :: IO ()
saveMaybe   = save "Maybe" static imports maybeDefns

Just static = (preludeClasses <:> maybeClasses) initialEnv

imports    :: [Assump]
imports     = defnsHaskellPrims ++ defnsHaskellPrelude

-----------------------------------------------------------------------------
-- Test Program:

maybeDefns :: [BindGroup]
maybeDefns
 = map toBg
   [[("isJust",
      Just (Forall [Star]
             ([] :=> 
              (TAp tMaybe (TGen 0) `fn` tBool))),
      [([PCon justCfun [PVar "a"]],
        econst trueCfun),
       ([PCon nothingCfun []],
        econst falseCfun)])],
    [("isNothing",
      Just (Forall [Star]
             ([] :=> 
              (TAp tMaybe (TGen 0) `fn` tBool))),
      [([PCon nothingCfun []],
        econst trueCfun),
       ([PCon justCfun [PVar "a"]],
        econst falseCfun)])],
    [("fromJust",
      Just (Forall [Star]
             ([] :=> 
              (TAp tMaybe (TGen 0) `fn` TGen 0))),
      [([PCon justCfun [PVar "a"]],
        evar "a"),
       ([PCon nothingCfun []],
        ap [evar "error", elit (LitStr "Maybe.fromJust: Nothing")])])],
    [("fromMaybe",
      Just (Forall [Star]
             ([] :=> 
              (TGen 0 `fn` TAp tMaybe (TGen 0) `fn` TGen 0))),
      [([PVar "d", PCon nothingCfun []],
        evar "d"),
       ([PVar "d", PCon justCfun [PVar "a"]],
        evar "a")])],
    [("maybeToList",
      Just (Forall [Star]
             ([] :=> 
              (TAp tMaybe (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PCon nothingCfun []],
        econst nilCfun),
       ([PCon justCfun [PVar "a"]],
        eCons (evar "a")
        eNil)])],
    [("listToMaybe",
      Just (Forall [Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tMaybe (TGen 0)))),
      [([PCon nilCfun []],
        econst nothingCfun),
       ([PCon consCfun [PVar "a", PVar "as"]],
        ap [econst justCfun, evar "a"])])],
    [("catMaybes",
      Just (Forall [Star]
             ([] :=> 
              (TAp tList (TAp tMaybe (TGen 0)) `fn` TAp tList (TGen 0)))),
      [([PVar "ms"],
        eCompFrom (PCon justCfun [PVar "m"]) (evar "ms")
        (eListRet (evar "m")))])],
    [("mapMaybe",
      Just (Forall [Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TAp tMaybe (TGen 1)) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))),
      [([PVar "f"],
        ap [evar ".", evar "catMaybes", ap [evar "map", evar "f"]])])]]

-----------------------------------------------------------------------------
