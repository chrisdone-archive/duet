-----------------------------------------------------------------------------
-- SourceList:		Haskell encoding of the List library source code
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

module SourceList where
import Testbed
import StaticList
import HaskellPrims
import HaskellPrelude
import HaskellMaybe

-----------------------------------------------------------------------------
-- Test Framework:

main       :: IO ()
main        = test static imports listDefns

saveList   :: IO ()
saveList    = save "List" static imports listDefns

Just static = (preludeClasses <:> maybeClasses <:> listClasses) initialEnv

imports    :: [Assump]
imports     = defnsHaskellPrims ++ defnsHaskellPrelude ++ defnsHaskellMaybe

-----------------------------------------------------------------------------
-- Test Program:

listDefns :: [BindGroup]
listDefns
 = map toBg
   [[("findIndices",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList tInt))),
      [([PVar "p", PVar "xs"],
        eCompFrom (PCon tup2Cfun [PVar "x", PVar "i"]) (ap [evar "zip", evar "xs", ap [econst enumFromMfun, elit (LitInt 0)]])
        (eCompGuard (ap [evar "p", evar "x"])
         (eListRet (evar "i"))))])],
    [("findIndex",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tMaybe tInt))),
      [([PVar "p"],
        ap [evar ".", evar "listToMaybe", ap [evar "findIndices", evar "p"]])])],
    [("elemIndex",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tMaybe tInt))),
      [([PVar "x"],
        ap [evar "findIndex", ap [econst eqMfun, evar "x"]])])],
    [("elemIndices",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList tInt))),
      [([PVar "x"],
        ap [evar "findIndices", ap [econst eqMfun, evar "x"]])])],
    [("find",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tMaybe (TGen 0)))),
      [([PVar "p"],
        ap [evar ".", evar "listToMaybe", ap [evar "filter", evar "p"]])])],
    [("nubBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "eq", PCon nilCfun []],
        econst nilCfun),
       ([PVar "eq", PCon consCfun [PVar "x", PVar "xs"]],
        ap [econst consCfun, evar "x", ap [evar "nubBy", evar "eq", ap [evar "filter", elambda ([PVar "y"],
                                                                                                ap [evar "not", ap [evar "eq", evar "x", evar "y"]]), evar "xs"]]])])],
    [("nub",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "nubBy", econst eqMfun])])],
    [("deleteBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "eq", PVar "x", PCon nilCfun []],
        econst nilCfun),
       ([PVar "eq", PVar "x", PCon consCfun [PVar "y", PVar "ys"]],
        eif (ap [evar "eq", evar "x", evar "y"])
            (evar "ys")
            (ap [econst consCfun, evar "y", ap [evar "deleteBy", evar "eq", evar "x", evar "ys"]]))])],
    [("delete",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "deleteBy", econst eqMfun])])],
    [("\\\\",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "foldl", ap [evar "flip", evar "delete"]])])],
    [("deleteFirstsBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "eq"],
        ap [evar "foldl", ap [evar "flip", ap [evar "deleteBy", evar "eq"]]])])],
    [("unionBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "eq", PVar "xs", PVar "ys"],
        ap [evar "++", evar "xs", ap [evar "foldl", ap [evar "flip", ap [evar "deleteBy", evar "eq"]], ap [evar "nubBy", evar "eq", evar "ys"], evar "xs"]])])],
    [("union",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "unionBy", econst eqMfun])])],
    [("intersectBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "eq", PVar "xs", PVar "ys"],
        eCompFrom (PVar "x") (evar "xs")
        (eCompGuard (ap [evar "any", ap [evar "eq", evar "x"], evar "ys"])
         (eListRet (evar "x"))))])],
    [("intersect",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "intersectBy", econst eqMfun])])],
    [("intersperse",
      Just (Forall [Star]
             ([] :=> 
              (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "sep", PCon nilCfun []],
        econst nilCfun),
       ([PVar "sep", pCons (PVar "x") pNil],
        eCons (evar "x")
        eNil),
       ([PVar "sep", PCon consCfun [PVar "x", PVar "xs"]],
        ap [econst consCfun, evar "x", ap [econst consCfun, evar "sep", ap [evar "intersperse", evar "sep", evar "xs"]]])])],
    [("transpose",
      Just (Forall [Star]
             ([] :=> 
              (TAp tList (TAp tList (TGen 0)) `fn` TAp tList (TAp tList (TGen 0))))),
      [([PCon nilCfun []],
        econst nilCfun),
       ([PCon consCfun [PCon nilCfun [], PVar "xss"]],
        ap [evar "transpose", evar "xss"]),
       ([PCon consCfun [PCon consCfun [PVar "x", PVar "xs"], PVar "xss"]],
        ap [econst consCfun, ap [econst consCfun, evar "x", eCompFrom (PCon consCfun [PVar "h", PVar "t"]) (evar "xss")
                                                            (eListRet (evar "h"))], ap [evar "transpose", ap [econst consCfun, evar "xs", eCompFrom (PCon consCfun [PVar "h", PVar "t"]) (evar "xss")
                                                                                                                                          (eListRet (evar "t"))]]])])],
    [("partition",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0))))),
      [([PVar "p", PVar "xs"],
        elet [[("select",
                Nothing,
                [([PVar "x", PCon tup2Cfun [PVar "ts", PVar "fs"]],
                  eguarded [(ap [evar "p", evar "x"],
                             ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "ts"], evar "fs"]),
                            (evar "otherwise",
                             ap [econst tup2Cfun, evar "ts", ap [econst consCfun, evar "x", evar "fs"]])])])]]
             (ap [evar "foldr", evar "select", ap [econst tup2Cfun, econst nilCfun, econst nilCfun], evar "xs"]))])],
    [("groupBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0))))),
      [([PVar "eq", PCon nilCfun []],
        econst nilCfun),
       ([PVar "eq", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v1919",
                Nothing,
                [([], ap [evar "span", ap [evar "eq", evar "x"], evar "xs"])]),
               ("ys",
                Nothing,
                [([], ecase (evar "v1919") [(PCon tup2Cfun [PVar "ys", PVar "zs"], evar "ys")])]),
               ("zs",
                Nothing,
                [([], ecase (evar "v1919") [(PCon tup2Cfun [PVar "ys", PVar "zs"], evar "zs")])])]]
             (ap [econst consCfun, ap [econst consCfun, evar "x", evar "ys"], ap [evar "groupBy", evar "eq", evar "zs"]]))])],
    [("group",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0))))),
      [([],
        ap [evar "groupBy", econst eqMfun])])],
    [("inits",
      Just (Forall [Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0))))),
      [([PCon nilCfun []],
        eCons (econst nilCfun)
        eNil),
       ([PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "++", eCons (econst nilCfun)
                       eNil, ap [evar "map", ap [econst consCfun, evar "x"], ap [evar "inits", evar "xs"]]])])],
    [("tails",
      Just (Forall [Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0))))),
      [([PCon nilCfun []],
        eCons (econst nilCfun)
        eNil),
       ([PAs "xxs" (PCon consCfun [PWildcard, PVar "xs"])],
        ap [econst consCfun, evar "xxs", ap [evar "tails", evar "xs"]])])],
    [("isPrefixOf",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tBool))),
      [([PCon nilCfun [], PWildcard],
        econst trueCfun),
       ([PWildcard, PCon nilCfun []],
        econst falseCfun),
       ([PCon consCfun [PVar "x", PVar "xs"], PCon consCfun [PVar "y", PVar "ys"]],
        ap [evar "&&", ap [econst eqMfun, evar "x", evar "y"], ap [evar "isPrefixOf", evar "xs", evar "ys"]])])],
    [("isSuffixOf",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tBool))),
      [([PVar "x", PVar "y"],
        ap [evar "isPrefixOf", ap [evar "reverse", evar "x"], ap [evar "reverse", evar "y"]])])],
    [("mapAccumL",
      Just (Forall [Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 2)) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TAp tList (TGen 2))))),
      [([PVar "f", PVar "s", PCon nilCfun []],
        ap [econst tup2Cfun, evar "s", econst nilCfun]),
       ([PVar "f", PVar "s", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v1920",
                Nothing,
                [([], ap [evar "f", evar "s", evar "x"])]),
               ("s'",
                Nothing,
                [([], ecase (evar "v1920") [(PCon tup2Cfun [PVar "s'", PVar "y"], evar "s'")])]),
               ("y",
                Nothing,
                [([], ecase (evar "v1920") [(PCon tup2Cfun [PVar "s'", PVar "y"], evar "y")])])],
              [("v1921",
                Nothing,
                [([], ap [evar "mapAccumL", evar "f", evar "s'", evar "xs"])]),
               ("s''",
                Nothing,
                [([], ecase (evar "v1921") [(PCon tup2Cfun [PVar "s''", PVar "ys"], evar "s''")])]),
               ("ys",
                Nothing,
                [([], ecase (evar "v1921") [(PCon tup2Cfun [PVar "s''", PVar "ys"], evar "ys")])])]]
             (ap [econst tup2Cfun, evar "s''", ap [econst consCfun, evar "y", evar "ys"]]))])],
    [("mapAccumR",
      Just (Forall [Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 2)) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TAp tList (TGen 2))))),
      [([PVar "f", PVar "s", PCon nilCfun []],
        ap [econst tup2Cfun, evar "s", econst nilCfun]),
       ([PVar "f", PVar "s", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v1922",
                Nothing,
                [([], ap [evar "mapAccumR", evar "f", evar "s", evar "xs"])]),
               ("s'",
                Nothing,
                [([], ecase (evar "v1922") [(PCon tup2Cfun [PVar "s'", PVar "ys"], evar "s'")])]),
               ("ys",
                Nothing,
                [([], ecase (evar "v1922") [(PCon tup2Cfun [PVar "s'", PVar "ys"], evar "ys")])])],
              [("v1923",
                Nothing,
                [([], ap [evar "f", evar "s'", evar "x"])]),
               ("s''",
                Nothing,
                [([], ecase (evar "v1923") [(PCon tup2Cfun [PVar "s''", PVar "y"], evar "s''")])]),
               ("y",
                Nothing,
                [([], ecase (evar "v1923") [(PCon tup2Cfun [PVar "s''", PVar "y"], evar "y")])])]]
             (ap [econst tup2Cfun, evar "s''", ap [econst consCfun, evar "y", evar "ys"]]))])],
    [("unfoldr",
      Just (Forall [Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TAp tMaybe (TAp (TAp tTuple2 (TGen 1)) (TGen 0))) `fn` TGen 0 `fn` TAp tList (TGen 1)))),
      [([PVar "f", PVar "b"],
        ecase (ap [evar "f", evar "b"])
              [(PCon nothingCfun [],
                econst nilCfun),
               (PCon justCfun [PCon tup2Cfun [PVar "a", PVar "b"]],
                ap [econst consCfun, evar "a", ap [evar "unfoldr", evar "f", evar "b"]])])])],
    [("insertBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tOrdering) `fn` TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "cmp", PVar "x", PCon nilCfun []],
        eCons (evar "x")
        eNil),
       ([PVar "cmp", PVar "x", PAs "ys" (PCon consCfun [PVar "y", PVar "ys'"])],
        ecase (ap [evar "cmp", evar "x", evar "y"])
              [(PCon gTCfun [],
                ap [econst consCfun, evar "y", ap [evar "insertBy", evar "cmp", evar "x", evar "ys'"]]),
               (PWildcard,
                ap [econst consCfun, evar "x", evar "ys"])])])],
    [("sortBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` tOrdering) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "cmp"],
        ap [evar "foldr", ap [evar "insertBy", evar "cmp"], econst nilCfun])])],
    [("sort",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "sortBy", econst compareMfun])])],
    [("insert",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "insertBy", econst compareMfun])])],
    [("maximumBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0))),
      [([PVar "max", PCon nilCfun []],
        ap [evar "error", elit (LitStr "List.maximumBy: empty list")]),
       ([PVar "max", PVar "xs"],
        ap [evar "foldl1", evar "max", evar "xs"])])],
    [("minimumBy",
      Just (Forall [Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0))),
      [([PVar "min", PCon nilCfun []],
        ap [evar "error", elit (LitStr "List.minimumBy: empty list")]),
       ([PVar "min", PVar "xs"],
        ap [evar "foldl1", evar "min", evar "xs"])])],
    [("genericLength",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TAp tList (TGen 1) `fn` TGen 0))),
      [([PCon nilCfun []],
        elit (LitInt 0)),
       ([PCon consCfun [PVar "x", PVar "xs"]],
        ap [econst plusMfun, elit (LitInt 1), ap [evar "genericLength", evar "xs"]])])],
    [("genericTake",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 1)))),
      [([PLit (LitInt 0), PWildcard],
        econst nilCfun),
       ([PWildcard, PCon nilCfun []],
        econst nilCfun),
       ([PVar "n", PCon consCfun [PVar "x", PVar "xs"]],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [econst consCfun, evar "x", ap [evar "genericTake", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"]]),
                  (evar "otherwise",
                   ap [evar "error", elit (LitStr "List.genericTake: negative argument")])])])],
    [("genericDrop",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 1)))),
      [([PLit (LitInt 0), PVar "xs"],
        evar "xs"),
       ([PWildcard, PCon nilCfun []],
        econst nilCfun),
       ([PVar "n", PCon consCfun [PWildcard, PVar "xs"]],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [evar "genericDrop", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"]),
                  (evar "otherwise",
                   ap [evar "error", elit (LitStr "List.genericDrop: negative argument")])])])],
    [("genericSplitAt",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TAp tList (TGen 1))) (TAp tList (TGen 1))))),
      [([PLit (LitInt 0), PVar "xs"],
        ap [econst tup2Cfun, econst nilCfun, evar "xs"]),
       ([PWildcard, PCon nilCfun []],
        ap [econst tup2Cfun, econst nilCfun, econst nilCfun]),
       ([PVar "n", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v1924",
                Nothing,
                [([], ap [evar "genericSplitAt", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"])]),
               ("xs'",
                Nothing,
                [([], ecase (evar "v1924") [(PCon tup2Cfun [PVar "xs'", PVar "xs''"], evar "xs'")])]),
               ("xs''",
                Nothing,
                [([], ecase (evar "v1924") [(PCon tup2Cfun [PVar "xs'", PVar "xs''"], evar "xs''")])])]]
             (eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                         ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "xs'"], evar "xs''"]),
                        (evar "otherwise",
                         ap [evar "error", elit (LitStr "List.genericSplitAt: negative argument")])]))])],
    [("genericIndex",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TAp tList (TGen 1) `fn` TGen 0 `fn` TGen 1))),
      [([PCon consCfun [PVar "x", PWildcard], PLit (LitInt 0)],
        evar "x"),
       ([PCon consCfun [PWildcard, PVar "xs"], PVar "n"],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [evar "genericIndex", evar "xs", ap [econst minusMfun, evar "n", elit (LitInt 1)]]),
                  (evar "otherwise",
                   ap [evar "error", elit (LitStr "List.genericIndex: negative argument")])]),
       ([PWildcard, PWildcard],
        ap [evar "error", elit (LitStr "List.genericIndex: index too large")])])],
    [("genericReplicate",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0)] :=> 
              (TGen 0 `fn` TGen 1 `fn` TAp tList (TGen 1)))),
      [([PVar "n", PVar "x"],
        ap [evar "genericTake", evar "n", ap [evar "repeat", evar "x"]])])],
    [("zipWith4",
      Just (Forall [Star, Star, Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"], PCon consCfun [PVar "c", PVar "cs"], PCon consCfun [PVar "d", PVar "ds"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b", evar "c", evar "d"], ap [evar "zipWith4", evar "z", evar "as", evar "bs", evar "cs", evar "ds"]]),
       ([PWildcard, PWildcard, PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip4",
      Just (Forall [Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))))),
      [([],
        ap [evar "zipWith4", econst tup4Cfun])])],
    [("zipWith5",
      Just (Forall [Star, Star, Star, Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"], PCon consCfun [PVar "c", PVar "cs"], PCon consCfun [PVar "d", PVar "ds"], PCon consCfun [PVar "e", PVar "es"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b", evar "c", evar "d", evar "e"], ap [evar "zipWith5", evar "z", evar "as", evar "bs", evar "cs", evar "ds", evar "es"]]),
       ([PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip5",
      Just (Forall [Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))))),
      [([],
        ap [evar "zipWith5", econst tup5Cfun])])],
    [("zipWith6",
      Just (Forall [Star, Star, Star, Star, Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"], PCon consCfun [PVar "c", PVar "cs"], PCon consCfun [PVar "d", PVar "ds"], PCon consCfun [PVar "e", PVar "es"], PCon consCfun [PVar "f", PVar "fs"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b", evar "c", evar "d", evar "e", evar "f"], ap [evar "zipWith6", evar "z", evar "as", evar "bs", evar "cs", evar "ds", evar "es", evar "fs"]]),
       ([PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip6",
      Just (Forall [Star, Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5))))),
      [([],
        ap [evar "zipWith6", econst tup6Cfun])])],
    [("zipWith7",
      Just (Forall [Star, Star, Star, Star, Star, Star, Star, Star]
             ([] :=> 
              ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6 `fn` TGen 7) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6) `fn` TAp tList (TGen 7)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"], PCon consCfun [PVar "c", PVar "cs"], PCon consCfun [PVar "d", PVar "ds"], PCon consCfun [PVar "e", PVar "es"], PCon consCfun [PVar "f", PVar "fs"], PCon consCfun [PVar "g", PVar "gs"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b", evar "c", evar "d", evar "e", evar "f", evar "g"], ap [evar "zipWith7", evar "z", evar "as", evar "bs", evar "cs", evar "ds", evar "es", evar "fs", evar "gs"]]),
       ([PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip7",
      Just (Forall [Star, Star, Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) (TGen 6))))),
      [([],
        ap [evar "zipWith7", econst tup7Cfun])])],
    [("unzip4",
      Just (Forall [Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) `fn` TAp (TAp (TAp (TAp tTuple4 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup4Cfun [PVar "a", PVar "b", PVar "c", PVar "d"], PLazy (PCon tup4Cfun [PVar "as", PVar "bs", PVar "cs", PVar "ds"])],
                                   ap [econst tup4Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"], ap [econst consCfun, evar "c", evar "cs"], ap [econst consCfun, evar "d", evar "ds"]]), ap [econst tup4Cfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun]])])],
    [("unzip5",
      Just (Forall [Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup5Cfun [PVar "a", PVar "b", PVar "c", PVar "d", PVar "e"], PLazy (PCon tup5Cfun [PVar "as", PVar "bs", PVar "cs", PVar "ds", PVar "es"])],
                                   ap [econst tup5Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"], ap [econst consCfun, evar "c", evar "cs"], ap [econst consCfun, evar "d", evar "ds"], ap [econst consCfun, evar "e", evar "es"]]), ap [econst tup5Cfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun]])])],
    [("unzip6",
      Just (Forall [Star, Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) `fn` TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4))) (TAp tList (TGen 5))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup6Cfun [PVar "a", PVar "b", PVar "c", PVar "d", PVar "e", PVar "f"], PLazy (PCon tup6Cfun [PVar "as", PVar "bs", PVar "cs", PVar "ds", PVar "es", PVar "fs"])],
                                   ap [econst tup6Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"], ap [econst consCfun, evar "c", evar "cs"], ap [econst consCfun, evar "d", evar "ds"], ap [econst consCfun, evar "e", evar "es"], ap [econst consCfun, evar "f", evar "fs"]]), ap [econst tup6Cfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun]])])],
    [("unzip7",
      Just (Forall [Star, Star, Star, Star, Star, Star, Star]
             ([] :=> 
              (TAp tList (TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) (TGen 6)) `fn` TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4))) (TAp tList (TGen 5))) (TAp tList (TGen 6))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup7Cfun [PVar "a", PVar "b", PVar "c", PVar "d", PVar "e", PVar "f", PVar "g"], PLazy (PCon tup7Cfun [PVar "as", PVar "bs", PVar "cs", PVar "ds", PVar "es", PVar "fs", PVar "gs"])],
                                   ap [econst tup7Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"], ap [econst consCfun, evar "c", evar "cs"], ap [econst consCfun, evar "d", evar "ds"], ap [econst consCfun, evar "e", evar "es"], ap [econst consCfun, evar "f", evar "fs"], ap [econst consCfun, evar "g", evar "gs"]]), ap [econst tup7Cfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun, econst nilCfun]])])]]

-----------------------------------------------------------------------------
