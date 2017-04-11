-----------------------------------------------------------------------------
-- SourceMonad:		Haskell encoding of the Monad library source code
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

module SourceMonad where
import Testbed
import StaticMonad
import HaskellPrims
import HaskellPrelude

-----------------------------------------------------------------------------
-- Test Framework:

main      :: IO ()
main       = test static imports (monadDefns ++ monadMems)

saveMonad :: IO ()
saveMonad  = save "Monad" static imports monadDefns

Just static = (preludeClasses <:> monadClasses) initialEnv

imports   :: [Assump]
imports    = defnsHaskellPrims ++
             defnsHaskellPrelude

-----------------------------------------------------------------------------
-- Test Programs:

monadDefns :: [BindGroup]
monadDefns
 = map toBg
   [[("msum",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonadPlus (TGen 0)] :=> 
              (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TGen 1)))),
      [([],
        ap [evar "foldr", econst mplusMfun, econst mzeroMfun])])],
    [("join",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (TAp (TGen 0) (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TGen 1)))),
      [([PVar "x"],
        ap [econst mbindMfun, evar "x", evar "id"])])],
    [("when",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tBool `fn` TAp (TGen 0) tUnit `fn` TAp (TGen 0) tUnit))),
      [([PVar "p", PVar "s"],
        eif (evar "p")
            (evar "s")
            (ap [econst returnMfun, econst unitCfun]))])],
    [("unless",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tBool `fn` TAp (TGen 0) tUnit `fn` TAp (TGen 0) tUnit))),
      [([PVar "p", PVar "s"],
        ap [evar "when", ap [evar "not", evar "p"], evar "s"])])],
    [("liftM2",
      Just (Forall [Kfun Star Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TGen 3) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3)))),
      [([PVar "f"],
        elambda ([PVar "a", PVar "b"],
                 eCompFrom (PVar "a'") (evar "a")
                 (eCompFrom (PVar "b'") (evar "b")
                  (ap [econst returnMfun, ap [evar "f", evar "a'", evar "b'"]]))))])],
    [("ap",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (TAp (TGen 0) (TGen 1 `fn` TGen 2) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2)))),
      [([],
        ap [evar "liftM2", evar "$"])])],
    [("guard",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonadPlus (TGen 0)] :=> 
              (tBool `fn` TAp (TGen 0) tUnit))),
      [([PVar "p"],
        eif (evar "p")
            (ap [econst returnMfun, econst unitCfun])
            (econst mzeroMfun))])],
    [("mapAndUnzipM",
      Just (Forall [Kfun Star Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TGen 2)) (TGen 3))) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TAp tList (TGen 2))) (TAp tList (TGen 3)))))),
      [([PVar "f", PVar "xs"],
        ap [econst mbindMfun, ap [evar "sequence", ap [evar "map", evar "f", evar "xs"]], ap [evar ".", econst returnMfun, evar "unzip"]])])],
    [("zipWithM",
      Just (Forall [Kfun Star Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 3)) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) (TAp tList (TGen 3))))),
      [([PVar "f", PVar "xs", PVar "ys"],
        ap [evar "sequence", ap [evar "zipWith", evar "f", evar "xs", evar "ys"]])])],
    [("zipWithM_",
      Just (Forall [Kfun Star Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 3)) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) tUnit))),
      [([PVar "f", PVar "xs", PVar "ys"],
        ap [evar "sequence_", ap [evar "zipWith", evar "f", evar "xs", evar "ys"]])])],
    [("foldM",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 1)) `fn` TGen 1 `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) (TGen 1)))),
      [([PVar "f", PVar "a", PCon nilCfun []],
        ap [econst returnMfun, evar "a"]),
       ([PVar "f", PVar "a", PCon consCfun [PVar "x", PVar "xs"]],
        ap [econst mbindMfun, ap [evar "f", evar "a", evar "x"], elambda ([PVar "y"],
                                                                          ap [evar "foldM", evar "f", evar "y", evar "xs"])])])],
    [("filterM",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TAp (TGen 0) tBool) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp tList (TGen 1))))),
      [([PVar "p", PCon nilCfun []],
        ap [econst returnMfun, econst nilCfun]),
       ([PVar "p", PCon consCfun [PVar "x", PVar "xs"]],
        eCompFrom (PVar "b") (ap [evar "p", evar "x"])
        (eCompFrom (PVar "ys") (ap [evar "filterM", evar "p", evar "xs"])
         (ap [econst returnMfun, eif (evar "b")
                                     (ap [econst consCfun, evar "x", evar "ys"])
                                     (evar "ys")])))])],
    [("liftM",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2)))),
      [([PVar "f"],
        elambda ([PVar "a"],
                 eCompFrom (PVar "a'") (evar "a")
                 (ap [econst returnMfun, ap [evar "f", evar "a'"]])))])],
    [("liftM3",
      Just (Forall [Kfun Star Star, Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4)))),
      [([PVar "f"],
        elambda ([PVar "a", PVar "b", PVar "c"],
                 eCompFrom (PVar "a'") (evar "a")
                 (eCompFrom (PVar "b'") (evar "b")
                  (eCompFrom (PVar "c'") (evar "c")
                   (ap [econst returnMfun, ap [evar "f", evar "a'", evar "b'", evar "c'"]])))))])],
    [("liftM4",
      Just (Forall [Kfun Star Star, Star, Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4) `fn` TAp (TGen 0) (TGen 5)))),
      [([PVar "f"],
        elambda ([PVar "a", PVar "b", PVar "c", PVar "d"],
                 eCompFrom (PVar "a'") (evar "a")
                 (eCompFrom (PVar "b'") (evar "b")
                  (eCompFrom (PVar "c'") (evar "c")
                   (eCompFrom (PVar "d'") (evar "d")
                    (ap [econst returnMfun, ap [evar "f", evar "a'", evar "b'", evar "c'", evar "d'"]]))))))])],
    [("liftM5",
      Just (Forall [Kfun Star Star, Star, Star, Star, Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4) `fn` TAp (TGen 0) (TGen 5) `fn` TAp (TGen 0) (TGen 6)))),
      [([PVar "f"],
        elambda ([PVar "a", PVar "b", PVar "c", PVar "d", PVar "e"],
                 eCompFrom (PVar "a'") (evar "a")
                 (eCompFrom (PVar "b'") (evar "b")
                  (eCompFrom (PVar "c'") (evar "c")
                   (eCompFrom (PVar "d'") (evar "d")
                    (eCompFrom (PVar "e'") (evar "e")
                     (ap [econst returnMfun, ap [evar "f", evar "a'", evar "b'", evar "c'", evar "d'", evar "e'"]])))))))])]]

monadMems :: [BindGroup]
monadMems
 = map (\x -> toBg [x])
   [("v1923",
     Just (Forall [Star]
            ([isIn1 cMonadPlus tMaybe] :=> 
             (TAp tMaybe (TGen 0)))),
     [([],
       econst nothingCfun)]),
    ("v1924",
     Just (Forall [Star]
            ([isIn1 cMonadPlus tMaybe] :=> 
             (TAp tMaybe (TGen 0) `fn` TAp tMaybe (TGen 0) `fn` TAp tMaybe (TGen 0)))),
     [([PCon nothingCfun [], PVar "ys"],
       evar "ys"),
      ([PVar "xs", PVar "ys"],
       evar "xs")]),
    ("v1927",
     Just (Forall [Star]
            ([isIn1 cMonadPlus tList] :=> 
             (TAp tList (TGen 0)))),
     [([],
       econst nilCfun)]),
    ("v1928",
     Just (Forall [Star]
            ([isIn1 cMonadPlus tList] :=> 
             (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
     [([],
       evar "++")])]


-----------------------------------------------------------------------------
