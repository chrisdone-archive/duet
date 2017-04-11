-----------------------------------------------------------------------------
-- SourceThih:		Haskell encoding of Typing Haskell in Haskell source
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

module SourceThih where
import Testbed
import StaticThih
import HaskellPrims
import HaskellPrelude
import HaskellMaybe
import HaskellList
import HaskellMonad

-----------------------------------------------------------------------------
-- Test Framework:

main     :: IO ()
main      = test static imports (thihDefns ++ thihMems)

saveThih :: IO ()
saveThih  = save "Thih" static imports thihDefns

Just static = (preludeClasses
           <:> maybeClasses
           <:> listClasses
           <:> monadClasses
           <:> thihClasses) initialEnv

imports  :: [Assump]
imports   = defnsHaskellPrims ++
            defnsHaskellPrelude ++
            defnsHaskellMaybe ++
            defnsHaskellList ++
            defnsHaskellMonad

-----------------------------------------------------------------------------
-- Test Programs:

thihDefns :: [BindGroup]
thihDefns
 = map toBg
   [[("enumId",
      Just (Forall []
             ([] :=> 
              (tInt `fn` tId))),
      [([PVar "n"],
        ap [evar "++", elit (LitStr "v"), ap [econst showMfun, evar "n"]])])],
    [("tUnit",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "()"), econst starCfun]])])],
    [("tChar",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "Char"), econst starCfun]])])],
    [("tInt",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "Int"), econst starCfun]])])],
    [("tInteger",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "Integer"), econst starCfun]])])],
    [("tFloat",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "Float"), econst starCfun]])])],
    [("tDouble",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "Double"), econst starCfun]])])],
    [("tList",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "[]"), ap [econst kfunCfun, econst starCfun, econst starCfun]]])])],
    [("tArrow",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "(->)"), ap [econst kfunCfun, econst starCfun, ap [econst kfunCfun, econst starCfun, econst starCfun]]]])])],
    [("tTuple2",
      Nothing,
      [([],
        ap [econst tConCfun, ap [econst tyconCfun, elit (LitStr "(,)"), ap [econst kfunCfun, econst starCfun, ap [econst kfunCfun, econst starCfun, econst starCfun]]]])])],
    [("fn",
      Just (Forall []
             ([] :=> 
              (tType `fn` tType `fn` tType))),
      [([PVar "a", PVar "b"],
        ap [econst tApCfun, ap [econst tApCfun, evar "tArrow", evar "a"], evar "b"])])],
    [("list",
      Just (Forall []
             ([] :=> 
              (tType `fn` tType))),
      [([PVar "t"],
        ap [econst tApCfun, evar "tList", evar "t"])])],
    [("pair",
      Just (Forall []
             ([] :=> 
              (tType `fn` tType `fn` tType))),
      [([PVar "a", PVar "b"],
        ap [econst tApCfun, ap [econst tApCfun, evar "tTuple2", evar "a"], evar "b"])])],
    [("tString",
      Just (Forall []
             ([] :=> 
              tType)),
      [([],
        ap [evar "list", evar "tChar"])])],
    [("nullSubst",
      Just (Forall []
             ([] :=> 
              tSubst)),
      [([],
        econst nilCfun)])],
    [("+->",
      Just (Forall []
             ([] :=> 
              (tTyvar `fn` tType `fn` tSubst))),
      [([PVar "u", PVar "t"],
        eCons (ap [econst tup2Cfun, evar "u", evar "t"])
        eNil)])],
    [("@@",
      Just (Forall []
             ([] :=> 
              (tSubst `fn` tSubst `fn` tSubst))),
      [([PVar "s1", PVar "s2"],
        ap [evar "++", eCompFrom (PCon tup2Cfun [PVar "u", PVar "t"]) (evar "s2")
                       (eListRet (ap [econst tup2Cfun, evar "u", ap [econst applyMfun, evar "s1", evar "t"]])), evar "s1"])])],
    [("merge",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tSubst `fn` tSubst `fn` TAp (TGen 0) tSubst))),
      [([PVar "s1", PVar "s2"],
        elet [[("agree",
                Nothing,
                [([],
                  ap [evar "all", elambda ([PVar "v"],
                                           ap [econst eqMfun, ap [econst applyMfun, evar "s1", ap [econst tVarCfun, evar "v"]], ap [econst applyMfun, evar "s2", ap [econst tVarCfun, evar "v"]]]), ap [evar "intersect", ap [evar "map", evar "fst", evar "s1"], ap [evar "map", evar "fst", evar "s2"]]])])]]
             (eif (evar "agree")
                  (ap [econst returnMfun, ap [evar "++", evar "s1", evar "s2"]])
                  (ap [econst failMfun, elit (LitStr "merge fails")])))])],
    [("varBind",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tTyvar `fn` tType `fn` TAp (TGen 0) tSubst))),
      [([PVar "u", PVar "t"],
        eguarded [(ap [econst eqMfun, evar "t", ap [econst tVarCfun, evar "u"]],
                   ap [econst returnMfun, evar "nullSubst"]),
                  (ap [evar "elem", evar "u", ap [econst tvMfun, evar "t"]],
                   ap [econst failMfun, elit (LitStr "occurs check fails")]),
                  (ap [econst neqMfun, ap [econst kindMfun, evar "u"], ap [econst kindMfun, evar "t"]],
                   ap [econst failMfun, elit (LitStr "kinds do not match")]),
                  (evar "otherwise",
                   ap [econst returnMfun, ap [evar "+->", evar "u", evar "t"]])])])],
    [("mgu",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tType `fn` tType `fn` TAp (TGen 0) tSubst))),
      [([PCon tApCfun [PVar "l", PVar "r"], PCon tApCfun [PVar "l'", PVar "r'"]],
        eCompFrom (PVar "s1") (ap [evar "mgu", evar "l", evar "l'"])
        (eCompFrom (PVar "s2") (ap [evar "mgu", ap [econst applyMfun, evar "s1", evar "r"], ap [econst applyMfun, evar "s1", evar "r'"]])
         (ap [econst returnMfun, ap [evar "@@", evar "s2", evar "s1"]]))),
       ([PCon tVarCfun [PVar "u"], PVar "t"],
        ap [evar "varBind", evar "u", evar "t"]),
       ([PVar "t", PCon tVarCfun [PVar "u"]],
        ap [evar "varBind", evar "u", evar "t"]),
       ([PCon tConCfun [PVar "tc1"], PCon tConCfun [PVar "tc2"]],
        eguarded [(ap [econst eqMfun, evar "tc1", evar "tc2"],
                   ap [econst returnMfun, evar "nullSubst"])]),
       ([PVar "t1", PVar "t2"],
        ap [econst failMfun, elit (LitStr "types do not unify")])])],
    [("match",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tType `fn` tType `fn` TAp (TGen 0) tSubst))),
      [([PCon tApCfun [PVar "l", PVar "r"], PCon tApCfun [PVar "l'", PVar "r'"]],
        eCompFrom (PVar "sl") (ap [evar "match", evar "l", evar "l'"])
        (eCompFrom (PVar "sr") (ap [evar "match", evar "r", evar "r'"])
         (ap [evar "merge", evar "sl", evar "sr"]))),
       ([PCon tVarCfun [PVar "u"], PVar "t"],
        eguarded [(ap [econst eqMfun, ap [econst kindMfun, evar "u"], ap [econst kindMfun, evar "t"]],
                   ap [econst returnMfun, ap [evar "+->", evar "u", evar "t"]])]),
       ([PCon tConCfun [PVar "tc1"], PCon tConCfun [PVar "tc2"]],
        eguarded [(ap [econst eqMfun, evar "tc1", evar "tc2"],
                   ap [econst returnMfun, evar "nullSubst"])]),
       ([PVar "t1", PVar "t2"],
        ap [econst failMfun, elit (LitStr "types do not match")])])],
    [("lift",
      Nothing,
      [([PVar "m", PCon isInCfun [PVar "c", PVar "t"], PCon isInCfun [PVar "c'", PVar "t'"]],
        eguarded [(ap [econst eqMfun, evar "c", evar "c'"],
                   ap [evar "m", evar "t", evar "t'"]),
                  (evar "otherwise",
                   ap [econst failMfun, elit (LitStr "classes do not match")])])])],
    [("mguPred",
      Just (Forall []
             ([] :=> 
              (tPred `fn` tPred `fn` TAp tMaybe tSubst))),
      [([],
        ap [evar "lift", evar "mgu"])])],
    [("matchPred",
      Just (Forall []
             ([] :=> 
              (tPred `fn` tPred `fn` TAp tMaybe tSubst))),
      [([],
        ap [evar "lift", evar "match"])])],
    [("super",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tId `fn` TAp tList tId))),
      [([PVar "ce", PVar "i"],
        ecase (ap [econst classesSfun, evar "ce", evar "i"])
              [(PCon justCfun [PCon tup2Cfun [PVar "is", PVar "its"]],
                evar "is")])])],
    [("insts",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tId `fn` TAp tList tInst))),
      [([PVar "ce", PVar "i"],
        ecase (ap [econst classesSfun, evar "ce", evar "i"])
              [(PCon justCfun [PCon tup2Cfun [PVar "is", PVar "its"]],
                evar "its")])])],
    [("defined",
      Just (Forall [Star]
             ([] :=> 
              (TAp tMaybe (TGen 0) `fn` tBool))),
      [([PCon justCfun [PVar "x"]],
        econst trueCfun),
       ([PCon nothingCfun []],
        econst falseCfun)])],
    [("modify",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tId `fn` tClass `fn` tClassEnv))),
      [([PVar "ce", PVar "i", PVar "c"],
        ecase (evar "ce")
              [(PCon classEnvCfun [PVar "cs", PVar "ds"],
                ap [econst classEnvCfun, elambda ([PVar "j"],
                                                  eif (ap [econst eqMfun, evar "i", evar "j"])
                                                      (ap [econst justCfun, evar "c"])
                                                      (ap [econst classesSfun, evar "ce", evar "j"])), evar "ds"])])])],
    [("initialEnv",
      Just (Forall []
             ([] :=> 
              tClassEnv)),
      [([],
        ap [econst classEnvCfun, elambda ([PVar "i"],
                                          ap [econst failMfun, elit (LitStr "class not defined")]), eCons (evar "tInteger")
                                                                                                    (eCons (evar "tDouble")
                                                                                                    eNil)])])],
    [("<:>",
      Just (Forall []
             ([] :=> 
              (tEnvTransformer `fn` tEnvTransformer `fn` tEnvTransformer))),
      [([PVar "f", PVar "g", PVar "ce"],
        eCompFrom (PVar "ce'") (ap [evar "f", evar "ce"])
        (ap [evar "g", evar "ce'"]))])],
    [("addClass",
      Just (Forall []
             ([] :=> 
              (tId `fn` TAp tList tId `fn` tEnvTransformer))),
      [([PVar "i", PVar "is", PVar "ce"],
        eguarded [(ap [evar "defined", ap [econst classesSfun, evar "ce", evar "i"]],
                   ap [econst failMfun, elit (LitStr "class already defined")]),
                  (ap [evar "any", ap [evar ".", evar "not", ap [evar ".", evar "defined", ap [econst classesSfun, evar "ce"]]], evar "is"],
                   ap [econst failMfun, elit (LitStr "superclass not defined")]),
                  (evar "otherwise",
                   ap [econst returnMfun, ap [evar "modify", evar "ce", evar "i", ap [econst tup2Cfun, evar "is", econst nilCfun]]])])])],
    [("addCoreClasses",
      Just (Forall []
             ([] :=> 
              tEnvTransformer)),
      [([],
        ap [evar "<:>", ap [evar "addClass", elit (LitStr "Eq"), econst nilCfun], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Ord"), eCons (elit (LitStr "Eq"))
                                                                                                                                            eNil], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Show"), econst nilCfun], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Read"), econst nilCfun], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Bounded"), econst nilCfun], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Enum"), econst nilCfun], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Functor"), econst nilCfun], ap [evar "addClass", elit (LitStr "Monad"), econst nilCfun]]]]]]]])])],
    [("addNumClasses",
      Just (Forall []
             ([] :=> 
              tEnvTransformer)),
      [([],
        ap [evar "<:>", ap [evar "addClass", elit (LitStr "Num"), eCons (elit (LitStr "Eq"))
                                                                  (eCons (elit (LitStr "Show"))
                                                                  eNil)], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Real"), eCons (elit (LitStr "Num"))
                                                                                                                                     (eCons (elit (LitStr "Ord"))
                                                                                                                                     eNil)], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Fractional"), eCons (elit (LitStr "Num"))
                                                                                                                                                                                                              eNil], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Integral"), eCons (elit (LitStr "Real"))
                                                                                                                                                                                                                                                                                    (eCons (elit (LitStr "Enum"))
                                                                                                                                                                                                                                                                                    eNil)], ap [evar "<:>", ap [evar "addClass", elit (LitStr "RealFrac"), eCons (elit (LitStr "Real"))
                                                                                                                                                                                                                                                                                                                                                           (eCons (elit (LitStr "Fractional"))
                                                                                                                                                                                                                                                                                                                                                           eNil)], ap [evar "<:>", ap [evar "addClass", elit (LitStr "Floating"), eCons (elit (LitStr "Fractional"))
                                                                                                                                                                                                                                                                                                                                                                                                                                  eNil], ap [evar "addClass", elit (LitStr "RealFloat"), eCons (elit (LitStr "RealFrac"))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (eCons (elit (LitStr "Floating"))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         eNil)]]]]]]])])],
    [("addPreludeClasses",
      Just (Forall []
             ([] :=> 
              tEnvTransformer)),
      [([],
        ap [evar "<:>", evar "addCoreClasses", evar "addNumClasses"])])],
    [("overlap",
      Just (Forall []
             ([] :=> 
              (tPred `fn` tPred `fn` tBool))),
      [([PVar "p", PVar "q"],
        ap [evar "defined", ap [evar "mguPred", evar "p", evar "q"]])])],
    [("addInst",
      Just (Forall []
             ([] :=> 
              (TAp tList tPred `fn` tPred `fn` tEnvTransformer))),
      [([PVar "ps", PAs "p" (PCon isInCfun [PVar "i", PWildcard]), PVar "ce"],
        elet [[("its",
                Nothing,
                [([],
                  ap [evar "insts", evar "ce", evar "i"])])],
              [("qs",
                Nothing,
                [([],
                  eCompFrom (PCon qualifyCfun [PWildcard, PVar "q"]) (evar "its")
                  (eListRet (evar "q")))])],
              [("c",
                Nothing,
                [([],
                  ap [econst tup2Cfun, ap [evar "super", evar "ce", evar "i"], ap [econst consCfun, ap [econst qualifyCfun, evar "ps", evar "p"], evar "its"]])])]]
             (eguarded [(ap [evar "not", ap [evar "defined", ap [econst classesSfun, evar "ce", evar "i"]]],
                         ap [econst failMfun, elit (LitStr "no class for instance")]),
                        (ap [evar "any", ap [evar "overlap", evar "p"], evar "qs"],
                         ap [econst failMfun, elit (LitStr "overlapping instance")]),
                        (evar "otherwise",
                         ap [econst returnMfun, ap [evar "modify", evar "ce", evar "i", evar "c"]])]))])],
    [("exampleInsts",
      Just (Forall []
             ([] :=> 
              tEnvTransformer)),
      [([],
        ap [evar "<:>", evar "addPreludeClasses", ap [evar "<:>", ap [evar "addInst", econst nilCfun, ap [econst isInCfun, elit (LitStr "Ord"), evar "tUnit"]], ap [evar "<:>", ap [evar "addInst", econst nilCfun, ap [econst isInCfun, elit (LitStr "Ord"), evar "tChar"]], ap [evar "<:>", ap [evar "addInst", econst nilCfun, ap [econst isInCfun, elit (LitStr "Ord"), evar "tInt"]], ap [evar "addInst", eCons (ap [econst isInCfun, elit (LitStr "Ord"), ap [econst tVarCfun, ap [econst tyvarCfun, elit (LitStr "a"), econst starCfun]]])
                                                                                                                                                                                                                                                                                                                                                                                                               (eCons (ap [econst isInCfun, elit (LitStr "Ord"), ap [econst tVarCfun, ap [econst tyvarCfun, elit (LitStr "b"), econst starCfun]]])
                                                                                                                                                                                                                                                                                                                                                                                                               eNil), ap [econst isInCfun, elit (LitStr "Ord"), ap [evar "pair", ap [econst tVarCfun, ap [econst tyvarCfun, elit (LitStr "a"), econst starCfun]], ap [econst tVarCfun, ap [econst tyvarCfun, elit (LitStr "b"), econst starCfun]]]]]]]]])])],
    [("bySuper",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tPred `fn` TAp tList tPred))),
      [([PVar "ce", PAs "p" (PCon isInCfun [PVar "i", PVar "t"])],
        ap [econst consCfun, evar "p", ap [evar "concat", eCompFrom (PVar "i'") (ap [evar "super", evar "ce", evar "i"])
                                                          (eListRet (ap [evar "bySuper", evar "ce", ap [econst isInCfun, evar "i'", evar "t"]]))]])])],
    [("byInst",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tPred `fn` TAp tMaybe (TAp tList tPred)))),
      [([PVar "ce", PAs "p" (PCon isInCfun [PVar "i", PVar "t"])],
        elet [[("tryInst",
                Nothing,
                [([PCon qualifyCfun [PVar "ps", PVar "h"]],
                  eCompFrom (PVar "u") (ap [evar "matchPred", evar "h", evar "p"])
                  (ap [econst justCfun, ap [evar "map", ap [econst applyMfun, evar "u"], evar "ps"]]))])]]
             (ap [evar "msum", eCompFrom (PVar "it") (ap [evar "insts", evar "ce", evar "i"])
                               (eListRet (ap [evar "tryInst", evar "it"]))]))])],
    [("entail",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tPred `fn` tPred `fn` tBool))),
      [([PVar "ce", PVar "ps", PVar "p"],
        ap [evar "||", ap [evar "any", ap [evar "elem", evar "p"], ap [evar "map", ap [evar "bySuper", evar "ce"], evar "ps"]], ecase (ap [evar "byInst", evar "ce", evar "p"])
                                                                                                                                      [(PCon nothingCfun [],
                                                                                                                                        econst falseCfun),
                                                                                                                                       (PCon justCfun [PVar "qs"],
                                                                                                                                        ap [evar "all", ap [evar "entail", evar "ce", evar "ps"], evar "qs"])]])])],
    [("inHnf",
      Just (Forall []
             ([] :=> 
              (tPred `fn` tBool))),
      [([PCon isInCfun [PVar "c", PVar "t"]],
        elet [[("hnf",
                Nothing,
                [([PCon tVarCfun [PVar "v"]],
                  econst trueCfun),
                 ([PCon tConCfun [PVar "tc"]],
                  econst falseCfun),
                 ([PCon tApCfun [PVar "t", PWildcard]],
                  ap [evar "hnf", evar "t"])])]]
             (ap [evar "hnf", evar "t"]))])],
    [("toHnfs",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred)))),
      [([PVar "ce", PVar "ps"],
        eCompFrom (PVar "pss") (ap [evar "mapM", ap [evar "toHnf", evar "ce"], evar "ps"])
        (ap [econst returnMfun, ap [evar "concat", evar "pss"]]))]),
     ("toHnf",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` tPred `fn` TAp (TGen 0) (TAp tList tPred)))),
      [([PVar "ce", PVar "p"],
        eguarded [(ap [evar "inHnf", evar "p"],
                   ap [econst returnMfun, eCons (evar "p")
                                          eNil]),
                  (evar "otherwise",
                   ecase (ap [evar "byInst", evar "ce", evar "p"])
                         [(PCon nothingCfun [],
                           ap [econst failMfun, elit (LitStr "context reduction")]),
                          (PCon justCfun [PVar "ps"],
                           ap [evar "toHnfs", evar "ce", evar "ps"])])])])],
    [("simplify",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tPred `fn` TAp tList tPred))),
      [([PVar "ce"],
        elet [[("loop",
                Nothing,
                [([PVar "rs", PCon nilCfun []],
                  evar "rs"),
                 ([PVar "rs", PCon consCfun [PVar "p", PVar "ps"]],
                  eguarded [(ap [evar "entail", evar "ce", ap [evar "++", evar "rs", evar "ps"], evar "p"],
                             ap [evar "loop", evar "rs", evar "ps"]),
                            (evar "otherwise",
                             ap [evar "loop", ap [econst consCfun, evar "p", evar "rs"], evar "ps"])])])]]
             (ap [evar "loop", econst nilCfun]))])],
    [("reduce",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred)))),
      [([PVar "ce", PVar "ps"],
        eCompFrom (PVar "qs") (ap [evar "toHnfs", evar "ce", evar "ps"])
        (ap [econst returnMfun, ap [evar "simplify", evar "ce", evar "qs"]]))])],
    [("scEntail",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tPred `fn` tPred `fn` tBool))),
      [([PVar "ce", PVar "ps", PVar "p"],
        ap [evar "any", ap [evar "elem", evar "p"], ap [evar "map", ap [evar "bySuper", evar "ce"], evar "ps"]])])],
    [("quantify",
      Just (Forall []
             ([] :=> 
              (TAp tList tTyvar `fn` TAp tQual tType `fn` tScheme))),
      [([PVar "vs", PVar "qt"],
        elet [[("vs'",
                Nothing,
                [([],
                  eCompFrom (PVar "v") (ap [econst tvMfun, evar "qt"])
                  (eCompGuard (ap [evar "elem", evar "v", evar "vs"])
                   (eListRet (evar "v"))))])],
              [("ks",
                Nothing,
                [([],
                  ap [evar "map", econst kindMfun, evar "vs'"])])],
              [("s",
                Nothing,
                [([],
                  ap [evar "zip", evar "vs'", ap [evar "map", econst tGenCfun, ap [econst enumFromMfun, elit (LitInt 0)]]])])]]
             (ap [econst forallCfun, evar "ks", ap [econst applyMfun, evar "s", evar "qt"]]))])],
    [("toScheme",
      Just (Forall []
             ([] :=> 
              (tType `fn` tScheme))),
      [([PVar "t"],
        ap [econst forallCfun, econst nilCfun, ap [econst qualifyCfun, econst nilCfun, evar "t"]])])],
    [("find",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tId `fn` TAp tList tAssump `fn` TAp (TGen 0) tScheme))),
      [([PVar "i", PCon nilCfun []],
        ap [econst failMfun, ap [evar "++", elit (LitStr "unbound identifier: "), evar "i"]]),
       ([PVar "i", PCon consCfun [PCon assumeCfun [PVar "i'", PVar "sc"], PVar "as"]],
        eif (ap [econst eqMfun, evar "i", evar "i'"])
            (ap [econst returnMfun, evar "sc"])
            (ap [evar "find", evar "i", evar "as"]))])],
    [("runTI",
      Just (Forall [Star]
             ([] :=> 
              (TAp tTI (TGen 0) `fn` TGen 0))),
      [([PCon tICfun [PVar "f"]],
        elet [[("v2049",
                Nothing,
                [([], ap [evar "f", evar "nullSubst", elit (LitInt 0)])]),
               ("s",
                Nothing,
                [([], ecase (evar "v2049") [(PCon tup3Cfun [PVar "s", PVar "n", PVar "x"], evar "s")])]),
               ("n",
                Nothing,
                [([], ecase (evar "v2049") [(PCon tup3Cfun [PVar "s", PVar "n", PVar "x"], evar "n")])]),
               ("x",
                Nothing,
                [([], ecase (evar "v2049") [(PCon tup3Cfun [PVar "s", PVar "n", PVar "x"], evar "x")])])]]
             (evar "x"))])],
    [("getSubst",
      Just (Forall []
             ([] :=> 
              (TAp tTI tSubst))),
      [([],
        ap [econst tICfun, elambda ([PVar "s", PVar "n"],
                                    ap [econst tup3Cfun, evar "s", evar "n", evar "s"])])])],
    [("extSubst",
      Just (Forall []
             ([] :=> 
              (tSubst `fn` TAp tTI tUnit))),
      [([PVar "s'"],
        ap [econst tICfun, elambda ([PVar "s", PVar "n"],
                                    ap [econst tup3Cfun, ap [evar "@@", evar "s'", evar "s"], evar "n", econst unitCfun])])])],
    [("unify",
      Just (Forall []
             ([] :=> 
              (tType `fn` tType `fn` TAp tTI tUnit))),
      [([PVar "t1", PVar "t2"],
        eCompFrom (PVar "s") (evar "getSubst")
        (eCompFrom (PVar "u") (ap [evar "mgu", ap [econst applyMfun, evar "s", evar "t1"], ap [econst applyMfun, evar "s", evar "t2"]])
         (ap [evar "extSubst", evar "u"])))])],
    [("newTVar",
      Just (Forall []
             ([] :=> 
              (tKind `fn` TAp tTI tType))),
      [([PVar "k"],
        ap [econst tICfun, elambda ([PVar "s", PVar "n"],
                                    elet [[("v",
                                            Nothing,
                                            [([],
                                              ap [econst tyvarCfun, ap [evar "enumId", evar "n"], evar "k"])])]]
                                         (ap [econst tup3Cfun, evar "s", ap [econst plusMfun, evar "n", elit (LitInt 1)], ap [econst tVarCfun, evar "v"]]))])])],
    [("freshInst",
      Just (Forall []
             ([] :=> 
              (tScheme `fn` TAp tTI (TAp tQual tType)))),
      [([PCon forallCfun [PVar "ks", PVar "qt"]],
        eCompFrom (PVar "ts") (ap [evar "mapM", evar "newTVar", evar "ks"])
        (ap [econst returnMfun, ap [econst instMfun, evar "ts", evar "qt"]]))])],
    [("tiLit",
      Just (Forall []
             ([] :=> 
              (tLiteral `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) tType)))),
      [([PCon litCharCfun [PWildcard]],
        ap [econst returnMfun, ap [econst tup2Cfun, econst nilCfun, evar "tChar"]]),
       ([PCon litIntCfun [PWildcard]],
        eCompFrom (PVar "v") (ap [evar "newTVar", econst starCfun])
        (ap [econst returnMfun, ap [econst tup2Cfun, eCons (ap [econst isInCfun, elit (LitStr "Num"), evar "v"])
                                                     eNil, evar "v"]])),
       ([PCon litStrCfun [PWildcard]],
        ap [econst returnMfun, ap [econst tup2Cfun, econst nilCfun, evar "tString"]]),
       ([PCon litRatCfun [PWildcard]],
        eCompFrom (PVar "v") (ap [evar "newTVar", econst starCfun])
        (ap [econst returnMfun, ap [econst tup2Cfun, eCons (ap [econst isInCfun, elit (LitStr "Fractional"), evar "v"])
                                                     eNil, evar "v"]]))])],
    [("tiPat",
      Just (Forall []
             ([] :=> 
              (tPat `fn` TAp tTI (TAp (TAp (TAp tTuple3 (TAp tList tPred)) (TAp tList tAssump)) tType)))),
      [([PCon pVarCfun [PVar "i"]],
        eCompFrom (PVar "v") (ap [evar "newTVar", econst starCfun])
        (ap [econst returnMfun, ap [econst tup3Cfun, econst nilCfun, eCons (ap [econst assumeCfun, evar "i", ap [evar "toScheme", evar "v"]])
                                                                     eNil, evar "v"]])),
       ([PCon pWildcardCfun []],
        eCompFrom (PVar "v") (ap [evar "newTVar", econst starCfun])
        (ap [econst returnMfun, ap [econst tup3Cfun, econst nilCfun, econst nilCfun, evar "v"]])),
       ([PCon pAsCfun [PVar "i", PVar "pat"]],
        eCompFrom (PCon tup3Cfun [PVar "ps", PVar "as", PVar "t"]) (ap [evar "tiPat", evar "pat"])
        (ap [econst returnMfun, ap [econst tup3Cfun, evar "ps", ap [econst consCfun, ap [econst assumeCfun, evar "i", ap [evar "toScheme", evar "t"]], evar "as"], evar "t"]])),
       ([PCon pLitCfun [PVar "l"]],
        eCompFrom (PCon tup2Cfun [PVar "ps", PVar "t"]) (ap [evar "tiLit", evar "l"])
        (ap [econst returnMfun, ap [econst tup3Cfun, evar "ps", econst nilCfun, evar "t"]])),
       ([PCon pNpkCfun [PVar "i", PVar "k"]],
        eCompFrom (PVar "t") (ap [evar "newTVar", econst starCfun])
        (ap [econst returnMfun, ap [econst tup3Cfun, eCons (ap [econst isInCfun, elit (LitStr "Integral"), evar "t"])
                                                     eNil, eCons (ap [econst assumeCfun, evar "i", ap [evar "toScheme", evar "t"]])
                                                           eNil, evar "t"]])),
       ([PCon pConCfun [PCon assumeCfun [PVar "i", PVar "sc"], PVar "pats"]],
        eCompFrom (PCon tup3Cfun [PVar "ps", PVar "as", PVar "ts"]) (ap [evar "tiPats", evar "pats"])
        (eCompFrom (PVar "t'") (ap [evar "newTVar", econst starCfun])
         (eCompFrom (PCon qualifyCfun [PVar "qs", PVar "t"]) (ap [evar "freshInst", evar "sc"])
          (eCompFrom PWildcard (ap [evar "unify", evar "t", ap [evar "foldr", evar "fn", evar "t'", evar "ts"]])
           (ap [econst returnMfun, ap [econst tup3Cfun, ap [evar "++", evar "ps", evar "qs"], evar "as", evar "t'"]])))))]),
     ("tiPats",
      Just (Forall []
             ([] :=> 
              (TAp tList tPat `fn` TAp tTI (TAp (TAp (TAp tTuple3 (TAp tList tPred)) (TAp tList tAssump)) (TAp tList tType))))),
      [([PVar "pats"],
        eCompFrom (PVar "psasts") (ap [evar "mapM", evar "tiPat", evar "pats"])
        (eCompLet [[("ps",
                     Nothing,
                     [([],
                       ap [evar "concat", eCompFrom (PCon tup3Cfun [PVar "ps'", PWildcard, PWildcard]) (evar "psasts")
                                          (eListRet (evar "ps'"))])])],
                   [("as",
                     Nothing,
                     [([],
                       ap [evar "concat", eCompFrom (PCon tup3Cfun [PWildcard, PVar "as'", PWildcard]) (evar "psasts")
                                          (eListRet (evar "as'"))])])],
                   [("ts",
                     Nothing,
                     [([],
                       eCompFrom (PCon tup3Cfun [PWildcard, PWildcard, PVar "t"]) (evar "psasts")
                       (eListRet (evar "t")))])]]
         (ap [econst returnMfun, ap [econst tup3Cfun, evar "ps", evar "as", evar "ts"]])))])],
    [("numClasses",
      Just (Forall []
             ([] :=> 
              (TAp tList tId))),
      [([],
        eCons (elit (LitStr "Num"))
        (eCons (elit (LitStr "Integral"))
        (eCons (elit (LitStr "Floating"))
        (eCons (elit (LitStr "Fractional"))
        (eCons (elit (LitStr "Real"))
        (eCons (elit (LitStr "RealFloat"))
        (eCons (elit (LitStr "RealFrac"))
        eNil)))))))])],
    [("stdClasses",
      Just (Forall []
             ([] :=> 
              (TAp tList tId))),
      [([],
        ap [evar "++", eCons (elit (LitStr "Eq"))
                       (eCons (elit (LitStr "Ord"))
                       (eCons (elit (LitStr "Show"))
                       (eCons (elit (LitStr "Read"))
                       (eCons (elit (LitStr "Bounded"))
                       (eCons (elit (LitStr "Enum"))
                       (eCons (elit (LitStr "Ix"))
                       (eCons (elit (LitStr "Functor"))
                       (eCons (elit (LitStr "Monad"))
                       (eCons (elit (LitStr "MonadPlus"))
                       eNil))))))))), evar "numClasses"])])],
    [("candidates",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` tAmbiguity `fn` TAp tList tType))),
      [([PVar "ce", PCon tup2Cfun [PVar "v", PVar "qs"]],
        eCompLet [[("is",
                    Nothing,
                    [([],
                      eCompFrom (PCon isInCfun [PVar "i", PVar "t"]) (evar "qs")
                      (eListRet (evar "i")))])],
                  [("ts",
                    Nothing,
                    [([],
                      eCompFrom (PCon isInCfun [PVar "i", PVar "t"]) (evar "qs")
                      (eListRet (evar "t")))])]]
        (eCompGuard (ap [evar "all", ap [econst eqMfun, ap [econst tVarCfun, evar "v"]], evar "ts"])
         (eCompGuard (ap [evar "any", ap [evar "flip", evar "elem", evar "numClasses"], evar "is"])
          (eCompGuard (ap [evar "all", ap [evar "flip", evar "elem", evar "stdClasses"], evar "is"])
           (eCompFrom (PVar "t'") (ap [econst defaultsSfun, evar "ce"])
            (eCompGuard (ap [evar "all", ap [evar "entail", evar "ce", econst nilCfun], eCompFrom (PVar "i") (evar "is")
                                                                                        (eListRet (ap [econst isInCfun, evar "i", evar "t'"]))])
             (eListRet (evar "t'"))))))))])],
    [("ambiguities",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp tList tAmbiguity))),
      [([PVar "ce", PVar "vs", PVar "ps"],
        eCompFrom (PVar "v") (ap [evar "\\\\", ap [econst tvMfun, evar "ps"], evar "vs"])
        (eListRet (ap [econst tup2Cfun, evar "v", ap [evar "filter", ap [evar ".", ap [evar "elem", evar "v"], econst tvMfun], evar "ps"]])))])],
    [("withDefaults",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              ((TAp tList tAmbiguity `fn` TAp tList tType `fn` TGen 1) `fn` tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TGen 1)))),
      [([PVar "f", PVar "ce", PVar "vs", PVar "ps"],
        elet [[("vps",
                Nothing,
                [([],
                  ap [evar "ambiguities", evar "ce", evar "vs", evar "ps"])])],
              [("tss",
                Nothing,
                [([],
                  ap [evar "map", ap [evar "candidates", evar "ce"], evar "vps"])])]]
             (eguarded [(ap [evar "any", evar "null", evar "tss"],
                         ap [econst failMfun, elit (LitStr "cannot resolve ambiguity")]),
                        (evar "otherwise",
                         ap [econst returnMfun, ap [evar "f", evar "vps", ap [evar "map", evar "head", evar "tss"]]])]))])],
    [("defaultedPreds",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred)))),
      [([],
        ap [evar "withDefaults", elambda ([PVar "vps", PVar "ts"],
                                          ap [evar "concat", ap [evar "map", evar "snd", evar "vps"]])])])],
    [("split",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tPred))))),
      [([PVar "ce", PVar "fs", PVar "gs", PVar "ps"],
        eCompFrom (PVar "ps'") (ap [evar "reduce", evar "ce", evar "ps"])
        (eCompLet [[("v2050",
                     Nothing,
                     [([], ap [evar "partition", ap [evar ".", ap [evar "all", ap [evar "flip", evar "elem", evar "fs"]], econst tvMfun], evar "ps'"])]),
                    ("ds",
                     Nothing,
                     [([], ecase (evar "v2050") [(PCon tup2Cfun [PVar "ds", PVar "rs"], evar "ds")])]),
                    ("rs",
                     Nothing,
                     [([], ecase (evar "v2050") [(PCon tup2Cfun [PVar "ds", PVar "rs"], evar "rs")])])]]
         (eCompFrom (PVar "rs'") (ap [evar "defaultedPreds", evar "ce", ap [evar "++", evar "fs", evar "gs"], evar "rs"])
          (ap [econst returnMfun, ap [econst tup2Cfun, evar "ds", ap [evar "\\\\", evar "rs", evar "rs'"]]]))))])],
    [("tiSeq",
      Just (Forall [Star]
             ([] :=> 
              (tInfer (TGen 0) (TAp tList tAssump) `fn` tInfer (TAp tList (TGen 0)) (TAp tList tAssump)))),
      [([PVar "ti", PVar "ce", PVar "as", PCon nilCfun []],
        ap [econst returnMfun, ap [econst tup2Cfun, econst nilCfun, econst nilCfun]]),
       ([PVar "ti", PVar "ce", PVar "as", PCon consCfun [PVar "bs", PVar "bss"]],
        eCompFrom (PCon tup2Cfun [PVar "ps", PVar "as'"]) (ap [evar "ti", evar "ce", evar "as", evar "bs"])
        (eCompFrom (PCon tup2Cfun [PVar "qs", PVar "as''"]) (ap [evar "tiSeq", evar "ti", evar "ce", ap [evar "++", evar "as'", evar "as"], evar "bss"])
         (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ps", evar "qs"], ap [evar "++", evar "as''", evar "as'"]]])))])],
    [("restricted",
      Just (Forall []
             ([] :=> 
              (TAp tList tImpl `fn` tBool))),
      [([PVar "bs"],
        elet [[("simple",
                Nothing,
                [([PCon tup2Cfun [PVar "i", PVar "alts"]],
                  ap [evar "any", ap [evar ".", evar "null", evar "fst"], evar "alts"])])]]
             (ap [evar "any", evar "simple", evar "bs"]))])],
    [("tiExpr",
      Just (Forall []
             ([] :=> 
              tInfer tExpr tType)),
      [([PVar "ce", PVar "as", PCon varCfun [PVar "i"]],
        eCompFrom (PVar "sc") (ap [evar "find", evar "i", evar "as"])
        (eCompFrom (PCon qualifyCfun [PVar "ps", PVar "t"]) (ap [evar "freshInst", evar "sc"])
         (ap [econst returnMfun, ap [econst tup2Cfun, evar "ps", evar "t"]]))),
       ([PVar "ce", PVar "as", PCon constCfun [PCon assumeCfun [PVar "i", PVar "sc"]]],
        eCompFrom (PCon qualifyCfun [PVar "ps", PVar "t"]) (ap [evar "freshInst", evar "sc"])
        (ap [econst returnMfun, ap [econst tup2Cfun, evar "ps", evar "t"]])),
       ([PVar "ce", PVar "as", PCon litCfun [PVar "l"]],
        eCompFrom (PCon tup2Cfun [PVar "ps", PVar "t"]) (ap [evar "tiLit", evar "l"])
        (ap [econst returnMfun, ap [econst tup2Cfun, evar "ps", evar "t"]])),
       ([PVar "ce", PVar "as", PCon apCfun [PVar "e", PVar "f"]],
        eCompFrom (PCon tup2Cfun [PVar "ps", PVar "te"]) (ap [evar "tiExpr", evar "ce", evar "as", evar "e"])
        (eCompFrom (PCon tup2Cfun [PVar "qs", PVar "tf"]) (ap [evar "tiExpr", evar "ce", evar "as", evar "f"])
         (eCompFrom (PVar "t") (ap [evar "newTVar", econst starCfun])
          (eCompFrom PWildcard (ap [evar "unify", ap [evar "fn", evar "tf", evar "t"], evar "te"])
           (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ps", evar "qs"], evar "t"]]))))),
       ([PVar "ce", PVar "as", PCon letCfun [PVar "bg", PVar "e"]],
        eCompFrom (PCon tup2Cfun [PVar "ps", PVar "as'"]) (ap [evar "tiBindGroup", evar "ce", evar "as", evar "bg"])
        (eCompFrom (PCon tup2Cfun [PVar "qs", PVar "t"]) (ap [evar "tiExpr", evar "ce", ap [evar "++", evar "as'", evar "as"], evar "e"])
         (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ps", evar "qs"], evar "t"]])))]),
     ("tiBindGroup",
      Just (Forall []
             ([] :=> 
              tInfer tBindGroup (TAp tList tAssump))),
      [([PVar "ce", PVar "as", PCon tup2Cfun [PVar "es", PVar "iss"]],
        eCompLet [[("as'",
                    Nothing,
                    [([],
                      eCompFrom (PCon tup3Cfun [PVar "v", PVar "sc", PVar "alts"]) (evar "es")
                      (eListRet (ap [econst assumeCfun, evar "v", evar "sc"])))])]]
        (eCompFrom (PCon tup2Cfun [PVar "ps", PVar "as''"]) (ap [evar "tiSeq", evar "tiImpls", evar "ce", ap [evar "++", evar "as'", evar "as"], evar "iss"])
         (eCompFrom (PVar "qss") (ap [evar "mapM", ap [evar "tiExpl", evar "ce", ap [evar "++", evar "as''", ap [evar "++", evar "as'", evar "as"]]], evar "es"])
          (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ps", ap [evar "concat", evar "qss"]], ap [evar "++", evar "as''", evar "as'"]]]))))]),
     ("tiExpl",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tAssump `fn` tExpl `fn` TAp tTI (TAp tList tPred)))),
      [([PVar "ce", PVar "as", PCon tup3Cfun [PVar "i", PVar "sc", PVar "alts"]],
        eCompFrom (PCon qualifyCfun [PVar "qs", PVar "t"]) (ap [evar "freshInst", evar "sc"])
        (eCompFrom (PVar "ps") (ap [evar "tiAlts", evar "ce", evar "as", evar "alts", evar "t"])
         (eCompFrom (PVar "s") (evar "getSubst")
          (eCompLet [[("qs'",
                       Nothing,
                       [([],
                         ap [econst applyMfun, evar "s", evar "qs"])])],
                     [("t'",
                       Nothing,
                       [([],
                         ap [econst applyMfun, evar "s", evar "t"])])],
                     [("fs",
                       Nothing,
                       [([],
                         ap [econst tvMfun, ap [econst applyMfun, evar "s", evar "as"]])])],
                     [("gs",
                       Nothing,
                       [([],
                         ap [evar "\\\\", ap [econst tvMfun, evar "t'"], evar "fs"])])],
                     [("sc'",
                       Nothing,
                       [([],
                         ap [evar "quantify", evar "gs", ap [econst qualifyCfun, evar "qs'", evar "t'"]])])],
                     [("ps'",
                       Nothing,
                       [([],
                         ap [evar "filter", ap [evar ".", evar "not", ap [evar "entail", evar "ce", evar "qs'"]], ap [econst applyMfun, evar "s", evar "ps"]])])]]
           (eCompFrom (PCon tup2Cfun [PVar "ds", PVar "rs"]) (ap [evar "split", evar "ce", evar "fs", evar "gs", evar "ps'"])
            (eif (ap [econst neqMfun, evar "sc", evar "sc'"])
                 (ap [econst failMfun, elit (LitStr "signature too general")])
                 (eif (ap [evar "not", ap [evar "null", evar "rs"]])
                      (ap [econst failMfun, elit (LitStr "context too weak")])
                      (ap [econst returnMfun, evar "ds"]))))))))]),
     ("tiAlts",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tAssump `fn` TAp tList tAlt `fn` tType `fn` TAp tTI (TAp tList tPred)))),
      [([PVar "ce", PVar "as", PVar "alts", PVar "t"],
        eCompFrom (PVar "psts") (ap [evar "mapM", ap [evar "tiAlt", evar "ce", evar "as"], evar "alts"])
        (eCompFrom PWildcard (ap [evar "mapM", ap [evar "unify", evar "t"], ap [evar "map", evar "snd", evar "psts"]])
         (ap [econst returnMfun, ap [evar "concat", ap [evar "map", evar "fst", evar "psts"]]])))]),
     ("tiAlt",
      Just (Forall []
             ([] :=> 
              tInfer tAlt tType)),
      [([PVar "ce", PVar "as", PCon tup2Cfun [PVar "pats", PVar "e"]],
        eCompFrom (PCon tup3Cfun [PVar "ps", PVar "as'", PVar "ts"]) (ap [evar "tiPats", evar "pats"])
        (eCompFrom (PCon tup2Cfun [PVar "qs", PVar "t"]) (ap [evar "tiExpr", evar "ce", ap [evar "++", evar "as'", evar "as"], evar "e"])
         (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ps", evar "qs"], ap [evar "foldr", evar "fn", evar "t", evar "ts"]]])))]),
     ("tiImpls",
      Just (Forall []
             ([] :=> 
              tInfer (TAp tList tImpl) (TAp tList tAssump))),
      [([PVar "ce", PVar "as", PVar "bs"],
        eCompFrom (PVar "ts") (ap [evar "mapM", elambda ([PWildcard],
                                                         ap [evar "newTVar", econst starCfun]), evar "bs"])
        (eCompLet [[("is",
                     Nothing,
                     [([],
                       ap [evar "map", evar "fst", evar "bs"])])],
                   [("scs",
                     Nothing,
                     [([],
                       ap [evar "map", evar "toScheme", evar "ts"])])],
                   [("as'",
                     Nothing,
                     [([],
                       ap [evar "++", ap [evar "zipWith", econst assumeCfun, evar "is", evar "scs"], evar "as"])])],
                   [("altss",
                     Nothing,
                     [([],
                       ap [evar "map", evar "snd", evar "bs"])])]]
         (eCompFrom (PVar "pss") (ap [evar "sequence", ap [evar "zipWith", ap [evar "tiAlts", evar "ce", evar "as'"], evar "altss", evar "ts"]])
          (eCompFrom (PVar "s") (evar "getSubst")
           (eCompLet [[("ps'",
                        Nothing,
                        [([],
                          ap [econst applyMfun, evar "s", ap [evar "concat", evar "pss"]])])],
                      [("ts'",
                        Nothing,
                        [([],
                          ap [econst applyMfun, evar "s", evar "ts"])])],
                      [("fs",
                        Nothing,
                        [([],
                          ap [econst tvMfun, ap [econst applyMfun, evar "s", evar "as"]])])],
                      [("vss",
                        Nothing,
                        [([],
                          ap [evar "map", econst tvMfun, evar "ts'"])])],
                      [("gs",
                        Nothing,
                        [([],
                          ap [evar "\\\\", ap [evar "foldr1", evar "union", evar "vss"], evar "fs"])])]]
            (eCompFrom (PCon tup2Cfun [PVar "ds", PVar "rs"]) (ap [evar "split", evar "ce", evar "fs", ap [evar "foldr1", evar "intersect", evar "vss"], evar "ps'"])
             (eif (ap [evar "restricted", evar "bs"])
                  (elet [[("gs'",
                           Nothing,
                           [([],
                             ap [evar "\\\\", evar "gs", ap [econst tvMfun, evar "rs"]])])],
                         [("scs'",
                           Nothing,
                           [([],
                             ap [evar "map", ap [evar ".", ap [evar "quantify", evar "gs'"], ap [econst qualifyCfun, econst nilCfun]], evar "ts'"])])]]
                        (ap [econst returnMfun, ap [econst tup2Cfun, ap [evar "++", evar "ds", evar "rs"], ap [evar "zipWith", econst assumeCfun, evar "is", evar "scs'"]]]))
                  (elet [[("scs'",
                           Nothing,
                           [([],
                             ap [evar "map", ap [evar ".", ap [evar "quantify", evar "gs"], ap [econst qualifyCfun, evar "rs"]], evar "ts'"])])]]
                        (ap [econst returnMfun, ap [econst tup2Cfun, evar "ds", ap [evar "zipWith", econst assumeCfun, evar "is", evar "scs'"]]])))))))))])],
    [("defaultSubst",
      Just (Forall [Kfun Star Star]
             ([isIn1 cMonad (TGen 0)] :=> 
              (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) tSubst))),
      [([],
        ap [evar "withDefaults", elambda ([PVar "vps", PVar "ts"],
                                          ap [evar "zip", ap [evar "map", evar "fst", evar "vps"], evar "ts"])])])],
    [("tiProgram",
      Just (Forall []
             ([] :=> 
              (tClassEnv `fn` TAp tList tAssump `fn` tProgram `fn` TAp tList tAssump))),
      [([PVar "ce", PVar "as", PVar "bgs"],
        ap [evar "$", evar "runTI", eCompFrom (PCon tup2Cfun [PVar "ps", PVar "as'"]) (ap [evar "tiSeq", evar "tiBindGroup", evar "ce", evar "as", evar "bgs"])
                                    (eCompFrom (PVar "s") (evar "getSubst")
                                     (eCompFrom (PVar "rs") (ap [evar "reduce", evar "ce", ap [econst applyMfun, evar "s", evar "ps"]])
                                      (eCompFrom (PVar "s'") (ap [evar "defaultSubst", evar "ce", econst nilCfun, evar "rs"])
                                       (ap [econst returnMfun, ap [econst applyMfun, ap [evar "@@", evar "s'", evar "s"], evar "as'"]]))))])])]]


thihMems :: [BindGroup]
thihMems
 = map (\x -> toBg [x])
   [("v2058",
     Just (Forall []
            ([isIn1 cHasKind tTyvar] :=> 
             (tTyvar `fn` tKind))),
     [([PCon tyvarCfun [PVar "v", PVar "k"]],
       evar "k")]),
    ("v2060",
     Just (Forall []
            ([isIn1 cHasKind tTycon] :=> 
             (tTycon `fn` tKind))),
     [([PCon tyconCfun [PVar "v", PVar "k"]],
       evar "k")]),
    ("v2062",
     Just (Forall []
            ([isIn1 cHasKind tType] :=> 
             (tType `fn` tKind))),
     [([PCon tConCfun [PVar "tc"]],
       ap [econst kindMfun, evar "tc"]),
      ([PCon tVarCfun [PVar "u"]],
       ap [econst kindMfun, evar "u"]),
      ([PCon tApCfun [PVar "t", PWildcard]],
       ecase (ap [econst kindMfun, evar "t"])
             [(PCon kfunCfun [PWildcard, PVar "k"],
               evar "k")])]),
    ("v2064",
     Just (Forall []
            ([isIn1 cTypes tType] :=> 
             (tSubst `fn` tType `fn` tType))),
     [([PVar "s", PCon tVarCfun [PVar "u"]],
       ecase (ap [evar "lookup", evar "u", evar "s"])
             [(PCon justCfun [PVar "t"],
               evar "t"),
              (PCon nothingCfun [],
               ap [econst tVarCfun, evar "u"])]),
      ([PVar "s", PCon tApCfun [PVar "l", PVar "r"]],
       ap [econst tApCfun, ap [econst applyMfun, evar "s", evar "l"], ap [econst applyMfun, evar "s", evar "r"]]),
      ([PVar "s", PVar "t"],
       evar "t")]),
    ("v2065",
     Just (Forall []
            ([isIn1 cTypes tType] :=> 
             (tType `fn` TAp tList tTyvar))),
     [([PCon tVarCfun [PVar "u"]],
       eCons (evar "u")
       eNil),
      ([PCon tApCfun [PVar "l", PVar "r"]],
       ap [evar "union", ap [econst tvMfun, evar "l"], ap [econst tvMfun, evar "r"]]),
      ([PVar "t"],
       econst nilCfun)]),
    ("v2067",
     Just (Forall [Star]
            ([isIn1 cTypes (TGen 0)] :=> 
             (tSubst `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
     [([PVar "s"],
       ap [evar "map", ap [econst applyMfun, evar "s"]])]),
    ("v2068",
     Just (Forall [Star]
            ([isIn1 cTypes (TGen 0)] :=> 
             (TAp tList (TGen 0) `fn` TAp tList tTyvar))),
     [([],
       ap [evar ".", evar "nub", ap [evar ".", evar "concat", ap [evar "map", econst tvMfun]]])]),
    ("v2070",
     Just (Forall [Star]
            ([isIn1 cTypes (TGen 0)] :=> 
             (tSubst `fn` TAp tQual (TGen 0) `fn` TAp tQual (TGen 0)))),
     [([PVar "s", PCon qualifyCfun [PVar "ps", PVar "t"]],
       ap [econst qualifyCfun, ap [econst applyMfun, evar "s", evar "ps"], ap [econst applyMfun, evar "s", evar "t"]])]),
    ("v2071",
     Just (Forall [Star]
            ([isIn1 cTypes (TGen 0)] :=> 
             (TAp tQual (TGen 0) `fn` TAp tList tTyvar))),
     [([PCon qualifyCfun [PVar "ps", PVar "t"]],
       ap [evar "union", ap [econst tvMfun, evar "ps"], ap [econst tvMfun, evar "t"]])]),
    ("v2073",
     Just (Forall []
            ([isIn1 cTypes tPred] :=> 
             (tSubst `fn` tPred `fn` tPred))),
     [([PVar "s", PCon isInCfun [PVar "i", PVar "t"]],
       ap [econst isInCfun, evar "i", ap [econst applyMfun, evar "s", evar "t"]])]),
    ("v2074",
     Just (Forall []
            ([isIn1 cTypes tPred] :=> 
             (tPred `fn` TAp tList tTyvar))),
     [([PCon isInCfun [PVar "i", PVar "t"]],
       ap [econst tvMfun, evar "t"])]),
    ("v2076",
     Just (Forall []
            ([isIn1 cTypes tScheme] :=> 
             (tSubst `fn` tScheme `fn` tScheme))),
     [([PVar "s", PCon forallCfun [PVar "ks", PVar "qt"]],
       ap [econst forallCfun, evar "ks", ap [econst applyMfun, evar "s", evar "qt"]])]),
    ("v2077",
     Just (Forall []
            ([isIn1 cTypes tScheme] :=> 
             (tScheme `fn` TAp tList tTyvar))),
     [([PCon forallCfun [PVar "ks", PVar "qt"]],
       ap [econst tvMfun, evar "qt"])]),
    ("v2079",
     Just (Forall []
            ([isIn1 cTypes tAssump] :=> 
             (tSubst `fn` tAssump `fn` tAssump))),
     [([PVar "s", PCon assumeCfun [PVar "i", PVar "sc"]],
       ap [econst assumeCfun, evar "i", ap [econst applyMfun, evar "s", evar "sc"]])]),
    ("v2080",
     Just (Forall []
            ([isIn1 cTypes tAssump] :=> 
             (tAssump `fn` TAp tList tTyvar))),
     [([PCon assumeCfun [PVar "i", PVar "sc"]],
       ap [econst tvMfun, evar "sc"])]),
    ("v2082",
     Just (Forall [Star]
            ([isIn1 cMonad tTI] :=> 
             (TGen 0 `fn` TAp tTI (TGen 0)))),
     [([PVar "x"],
       ap [econst tICfun, elambda ([PVar "s", PVar "n"],
                                   ap [econst tup3Cfun, evar "s", evar "n", evar "x"])])]),
    ("v2083",
     Just (Forall [Star, Star]
            ([isIn1 cMonad tTI] :=> 
             (TAp tTI (TGen 0) `fn` (TGen 0 `fn` TAp tTI (TGen 1)) `fn` TAp tTI (TGen 1)))),
     [([PCon tICfun [PVar "f"], PVar "g"],
       ap [econst tICfun, elambda ([PVar "s", PVar "n"],
                                   ecase (ap [evar "f", evar "s", evar "n"])
                                         [(PCon tup3Cfun [PVar "s'", PVar "m", PVar "x"],
                                           elet [[("v2084",
                                                   Nothing,
                                                   [([], ap [evar "g", evar "x"])]),
                                                  ("gx",
                                                   Nothing,
                                                   [([], ecase (evar "v2084") [(PCon tICfun [PVar "gx"], evar "gx")])])]]
                                                (ap [evar "gx", evar "s'", evar "m"]))])])]),
    ("v2088",
     Just (Forall []
            ([isIn1 cInstantiate tType] :=> 
             (TAp tList tType `fn` tType `fn` tType))),
     [([PVar "ts", PCon tApCfun [PVar "l", PVar "r"]],
       ap [econst tApCfun, ap [econst instMfun, evar "ts", evar "l"], ap [econst instMfun, evar "ts", evar "r"]]),
      ([PVar "ts", PCon tGenCfun [PVar "n"]],
       ap [evar "!!", evar "ts", evar "n"]),
      ([PVar "ts", PVar "t"],
       evar "t")]),
    ("v2090",
     Just (Forall [Star]
            ([isIn1 cInstantiate (TGen 0)] :=> 
             (TAp tList tType `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
     [([PVar "ts"],
       ap [evar "map", ap [econst instMfun, evar "ts"]])]),
    ("v2092",
     Just (Forall [Star]
            ([isIn1 cInstantiate (TGen 0)] :=> 
             (TAp tList tType `fn` TAp tQual (TGen 0) `fn` TAp tQual (TGen 0)))),
     [([PVar "ts", PCon qualifyCfun [PVar "ps", PVar "t"]],
       ap [econst qualifyCfun, ap [econst instMfun, evar "ts", evar "ps"], ap [econst instMfun, evar "ts", evar "t"]])]),
    ("v2094",
     Just (Forall []
            ([isIn1 cInstantiate tPred] :=> 
             (TAp tList tType `fn` tPred `fn` tPred))),
     [([PVar "ts", PCon isInCfun [PVar "c", PVar "t"]],
       ap [econst isInCfun, evar "c", ap [econst instMfun, evar "ts", evar "t"]])]),
    ("v2096",
     Just (Forall []
            ([isIn1 cEq tKind] :=> 
             (tKind `fn` tKind `fn` tBool))),
     [([PCon starCfun [], PCon starCfun []],
       econst trueCfun),
      ([PCon kfunCfun [PVar "v2039", PVar "v2038"], PCon kfunCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]]),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v2099",
     Just (Forall []
            ([isIn1 cEq tType] :=> 
             (tType `fn` tType `fn` tBool))),
     [([PCon tVarCfun [PVar "v2040"], PCon tVarCfun [PVar "v2041"]],
       ap [econst eqMfun, evar "v2040", evar "v2041"]),
      ([PCon tConCfun [PVar "v2040"], PCon tConCfun [PVar "v2041"]],
       ap [econst eqMfun, evar "v2040", evar "v2041"]),
      ([PCon tApCfun [PVar "v2039", PVar "v2038"], PCon tApCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]]),
      ([PCon tGenCfun [PVar "v2040"], PCon tGenCfun [PVar "v2041"]],
       ap [econst eqMfun, evar "v2040", evar "v2041"]),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v2102",
     Just (Forall []
            ([isIn1 cEq tTyvar] :=> 
             (tTyvar `fn` tTyvar `fn` tBool))),
     [([PCon tyvarCfun [PVar "v2039", PVar "v2038"], PCon tyvarCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]])]),
    ("v2105",
     Just (Forall []
            ([isIn1 cEq tTycon] :=> 
             (tTycon `fn` tTycon `fn` tBool))),
     [([PCon tyconCfun [PVar "v2039", PVar "v2038"], PCon tyconCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]])]),
    ("v2108",
     Just (Forall [Star]
            ([isIn1 cEq (TGen 0)] :=> 
             (TAp tQual (TGen 0) `fn` TAp tQual (TGen 0) `fn` tBool))),
     [([PCon qualifyCfun [PVar "v2039", PVar "v2038"], PCon qualifyCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]])]),
    ("v2111",
     Just (Forall []
            ([isIn1 cEq tPred] :=> 
             (tPred `fn` tPred `fn` tBool))),
     [([PCon isInCfun [PVar "v2039", PVar "v2038"], PCon isInCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]])]),
    ("v2114",
     Just (Forall []
            ([isIn1 cEq tScheme] :=> 
             (tScheme `fn` tScheme `fn` tBool))),
     [([PCon forallCfun [PVar "v2039", PVar "v2038"], PCon forallCfun [PVar "v2041", PVar "v2040"]],
       ap [evar "&&", ap [econst eqMfun, evar "v2039", evar "v2041"], ap [econst eqMfun, evar "v2038", evar "v2040"]])])]


-----------------------------------------------------------------------------
