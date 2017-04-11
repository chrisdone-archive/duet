-----------------------------------------------------------------------------
-- SourcePrelude:       Haskell encoding of the Hugs Prelude source code
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

module SourcePrelude where
import Testbed
import HaskellPrims
import StaticPrelude


savePrelude  :: IO ()
savePrelude   = save "Prelude"
                     static
                     defnsHaskellPrims
                     preludeDefns

Just static   = preludeClasses initialEnv

-----------------------------------------------------------------------------
-- Main definitions:

preludeDefns :: [BindGroup]
preludeDefns
 = map toBg
   [[("flip",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TGen 1 `fn` TGen 0 `fn` TGen 2))),
      [([PVar "f", PVar "x", PVar "y"],
        ap [evar "f", evar "y", evar "x"])])],
    [("subtract",
      Just (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0))),
      [([],
        ap [evar "flip", econst minusMfun])])],
    [("gcd",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0))),
      [([PLit (LitInt 0), PLit (LitInt 0)],
        ap [evar "error", elit (LitStr "Prelude.gcd: gcd 0 0 is undefined")]),
       ([PVar "x", PVar "y"],
        elet [[("gcd'",
                Nothing,
                [([PVar "x", PLit (LitInt 0)],
                  evar "x"),
                 ([PVar "x", PVar "y"],
                  ap [evar "gcd'", evar "y", ap [econst remMfun, evar "x", evar "y"]])])]]
             (ap [evar "gcd'", ap [econst absMfun, evar "x"], ap [econst absMfun, evar "y"]]))])],
    [("lcm",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0))),
      [([PWildcard, PLit (LitInt 0)],
        elit (LitInt 0)),
       ([PLit (LitInt 0), PWildcard],
        elit (LitInt 0)),
       ([PVar "x", PVar "y"],
        ap [econst absMfun, ap [econst timesMfun, ap [econst quotMfun, evar "x", ap [evar "gcd", evar "x", evar "y"]], evar "y"]])])],
    [("otherwise",
      Just (Forall []
             ([] :=>
              tBool)),
      [([],
        econst trueCfun)])],
    [("^",
      Just (Forall [Star, Star]
             ([isIn1 cNum (TGen 0),
               isIn1 cIntegral (TGen 1)] :=>
              (TGen 0 `fn` TGen 1 `fn` TGen 0))),
      [([PVar "x", PLit (LitInt 0)],
        elit (LitInt 1)),
       ([PVar "x", PVar "n"],
        elet [[("f",
                Nothing,
                [([PWildcard, PLit (LitInt 0), PVar "y"],
                  evar "y"),
                 ([PVar "x", PVar "n", PVar "y"],
                  elet [[("g",
                          Nothing,
                          [([PVar "x", PVar "n"],
                            eguarded [(ap [econst evenMfun, evar "n"],
                                       ap [evar "g", ap [econst timesMfun, evar "x", evar "x"], ap [econst quotMfun, evar "n", elit (LitInt 2)]]),
                                      (evar "otherwise",
                                       ap [evar "f", evar "x", ap [econst minusMfun, evar "n", elit (LitInt 1)], ap [econst timesMfun, evar "x", evar "y"]])])])]]
                       (ap [evar "g", evar "x", evar "n"]))])]]
             (eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                         ap [evar "f", evar "x", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "x"])])),
       ([PWildcard, PWildcard],
        ap [evar "error", elit (LitStr "Prelude.^: negative exponent")])])],
    [("^^",
      Just (Forall [Star, Star]
             ([isIn1 cFractional (TGen 0),
               isIn1 cIntegral (TGen 1)] :=>
              (TGen 0 `fn` TGen 1 `fn` TGen 0))),
      [([PVar "x", PVar "n"],
        eif (ap [econst geMfun, evar "n", elit (LitInt 0)])
            (ap [evar "^", evar "x", evar "n"])
            (ap [econst recipMfun, ap [evar "^", evar "x", ap [econst negateMfun, evar "n"]]]))])],
    [(".",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1) `fn` (TGen 2 `fn` TGen 0) `fn` TGen 2 `fn` TGen 1))),
      [([PVar "f", PVar "g", PVar "x"],
        ap [evar "f", ap [evar "g", evar "x"]])])],
    [("fromIntegral",
      Just (Forall [Star, Star]
             ([isIn1 cIntegral (TGen 0),
               isIn1 cNum (TGen 1)] :=>
              (TGen 0 `fn` TGen 1))),
      [([],
        ap [evar ".", econst fromIntegerMfun, econst toIntegerMfun])])],
    [("realToFrac",
      Just (Forall [Star, Star]
             ([isIn1 cReal (TGen 0),
               isIn1 cFractional (TGen 1)] :=>
              (TGen 0 `fn` TGen 1))),
      [([],
        ap [evar ".", econst fromRationalMfun, econst toRationalMfun])])],
    [("sequence",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonad (TGen 0)] :=>
              (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TAp tList (TGen 1))))),
      [([PCon nilCfun []],
        ap [econst returnMfun, econst nilCfun]),
       ([PCon consCfun [PVar "c", PVar "cs"]],
        eCompFrom (PVar "x") (evar "c")
        (eCompFrom (PVar "xs") (ap [evar "sequence", evar "cs"])
         (ap [econst returnMfun, ap [econst consCfun, evar "x", evar "xs"]])))])],
    [("foldr",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` TAp tList (TGen 0) `fn` TGen 1))),
      [([PVar "f", PVar "z", PCon nilCfun []],
        evar "z"),
       ([PVar "f", PVar "z", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "f", evar "x", ap [evar "foldr", evar "f", evar "z", evar "xs"]])])],
    [("sequence_",
      Just (Forall [Kfun Star Star, Star]
             ([isIn1 cMonad (TGen 0)] :=>
              (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) tUnit))),
      [([],
        ap [evar "foldr", econst mthenMfun, ap [econst returnMfun, econst unitCfun]])])],
    [("map",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))),
      [([PVar "f", PVar "xs"],
        eCompFrom (PVar "x") (evar "xs")
        (eListRet (ap [evar "f", evar "x"])))])],
    [("mapM",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=>
              ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp tList (TGen 2))))),
      [([PVar "f"],
        ap [evar ".", evar "sequence", ap [evar "map", evar "f"]])])],
    [("mapM_",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=>
              ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) tUnit))),
      [([PVar "f"],
        ap [evar ".", evar "sequence_", ap [evar "map", evar "f"]])])],
    [("=<<",
      Just (Forall [Kfun Star Star, Star, Star]
             ([isIn1 cMonad (TGen 0)] :=>
              ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2)))),
      [([PVar "f", PVar "x"],
        ap [econst mbindMfun, evar "x", evar "f"])])],
    [("&&",
      Just (Forall []
             ([] :=>
              (tBool `fn` tBool `fn` tBool))),
      [([PCon falseCfun [], PVar "x"],
        econst falseCfun),
       ([PCon trueCfun [], PVar "x"],
        evar "x")])],
    [("||",
      Just (Forall []
             ([] :=>
              (tBool `fn` tBool `fn` tBool))),
      [([PCon falseCfun [], PVar "x"],
        evar "x"),
       ([PCon trueCfun [], PVar "x"],
        econst trueCfun)])],
    [("not",
      Just (Forall []
             ([] :=>
              (tBool `fn` tBool))),
      [([PCon trueCfun []],
        econst falseCfun),
       ([PCon falseCfun []],
        econst trueCfun)])],
    [("isAscii",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [econst ltMfun, ap [econst fromEnumMfun, evar "c"], elit (LitInt 128)])])],
    [("isControl",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "||", ap [econst ltMfun, evar "c", elit (LitChar ' ')], ap [econst eqMfun, evar "c", elit (LitChar '\DEL')]])])],
    [("isPrint",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar ' ')], ap [econst leMfun, evar "c", elit (LitChar '~')]])])],
    [("isSpace",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "||", ap [econst eqMfun, evar "c", elit (LitChar ' ')], ap [evar "||", ap [econst eqMfun, evar "c", elit (LitChar '\t')], ap [evar "||", ap [econst eqMfun, evar "c", elit (LitChar '\n')], ap [evar "||", ap [econst eqMfun, evar "c", elit (LitChar '\r')], ap [evar "||", ap [econst eqMfun, evar "c", elit (LitChar '\f')], ap [econst eqMfun, evar "c", elit (LitChar '\v')]]]]]])])],
    [("isUpper",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'A')], ap [econst leMfun, evar "c", elit (LitChar 'Z')]])])],
    [("isLower",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'a')], ap [econst leMfun, evar "c", elit (LitChar 'z')]])])],
    [("isAlpha",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "||", ap [evar "isUpper", evar "c"], ap [evar "isLower", evar "c"]])])],
    [("isDigit",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar '0')], ap [econst leMfun, evar "c", elit (LitChar '9')]])])],
    [("isAlphaNum",
      Just (Forall []
             ([] :=>
              (tChar `fn` tBool))),
      [([PVar "c"],
        ap [evar "||", ap [evar "isAlpha", evar "c"], ap [evar "isDigit", evar "c"]])])],
    [("digitToInt",
      Just (Forall []
             ([] :=>
              (tChar `fn` tInt))),
      [([PVar "c"],
        eguarded [(ap [evar "isDigit", evar "c"],
                   ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar '0')]]),
                  (ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'a')], ap [econst leMfun, evar "c", elit (LitChar 'f')]],
                   ap [econst plusMfun, ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar 'a')]], elit (LitInt 10)]),
                  (ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'A')], ap [econst leMfun, evar "c", elit (LitChar 'F')]],
                   ap [econst plusMfun, ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar 'A')]], elit (LitInt 10)]),
                  (evar "otherwise",
                   ap [evar "error", elit (LitStr "Char.digitToInt: not a digit")])])])],
    [("intToDigit",
      Just (Forall []
             ([] :=>
              (tInt `fn` tChar))),
      [([PVar "i"],
        eguarded [(ap [evar "&&", ap [econst geMfun, evar "i", elit (LitInt 0)], ap [econst leMfun, evar "i", elit (LitInt 9)]],
                   ap [econst toEnumMfun, ap [econst plusMfun, ap [econst fromEnumMfun, elit (LitChar '0')], evar "i"]]),
                  (ap [evar "&&", ap [econst geMfun, evar "i", elit (LitInt 10)], ap [econst leMfun, evar "i", elit (LitInt 15)]],
                   ap [econst toEnumMfun, ap [econst minusMfun, ap [econst plusMfun, ap [econst fromEnumMfun, elit (LitChar 'a')], evar "i"], elit (LitInt 10)]]),
                  (evar "otherwise",
                   ap [evar "error", elit (LitStr "Char.intToDigit: not a digit")])])])],
    [("toUpper",
      Just (Forall []
             ([] :=>
              (tChar `fn` tChar))),
      [([PVar "c"],
        eguarded [(ap [evar "isLower", evar "c"],
                   ap [econst toEnumMfun, ap [econst plusMfun, ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar 'a')]], ap [econst fromEnumMfun, elit (LitChar 'A')]]]),
                  (evar "otherwise",
                   evar "c")])])],
    [("toLower",
      Just (Forall []
             ([] :=>
              (tChar `fn` tChar))),
      [([PVar "c"],
        eguarded [(ap [evar "isUpper", evar "c"],
                   ap [econst toEnumMfun, ap [econst plusMfun, ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar 'A')]], ap [econst fromEnumMfun, elit (LitChar 'a')]]]),
                  (evar "otherwise",
                   evar "c")])])],
    [("ord",
      Just (Forall []
             ([] :=>
              (tChar `fn` tInt))),
      [([],
        econst fromEnumMfun)])],
    [("chr",
      Just (Forall []
             ([] :=>
              (tInt `fn` tChar))),
      [([],
        econst toEnumMfun)])],
    [("maybe",
      Just (Forall [Star, Star]
             ([] :=>
              (TGen 0 `fn` (TGen 1 `fn` TGen 0) `fn` TAp tMaybe (TGen 1) `fn` TGen 0))),
      [([PVar "n", PVar "f", PCon nothingCfun []],
        evar "n"),
       ([PVar "n", PVar "f", PCon justCfun [PVar "x"]],
        ap [evar "f", evar "x"])])],
    [("either",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1) `fn` (TGen 2 `fn` TGen 1) `fn` TAp (TAp tEither (TGen 0)) (TGen 2) `fn` TGen 1))),
      [([PVar "l", PVar "r", PCon leftCfun [PVar "x"]],
        ap [evar "l", evar "x"]),
       ([PVar "l", PVar "r", PCon rightCfun [PVar "y"]],
        ap [evar "r", evar "y"])])],
    [("absReal",
      Nothing,
      [([PVar "x"],
        eguarded [(ap [econst geMfun, evar "x", elit (LitInt 0)],
                   evar "x"),
                  (evar "otherwise",
                   ap [econst negateMfun, evar "x"])])])],
    [("signumReal",
      Nothing,
      [([PVar "x"],
        eguarded [(ap [econst eqMfun, evar "x", elit (LitInt 0)],
                   elit (LitInt 0)),
                  (ap [econst gtMfun, evar "x", elit (LitInt 0)],
                   elit (LitInt 1)),
                  (evar "otherwise",
                   elit (LitInt (-1)))])])],
    [("numericEnumFrom",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              (TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "n"],
        ap [econst consCfun, evar "n", ap [evar "$!", evar "numericEnumFrom", ap [econst plusMfun, evar "n", elit (LitInt 1)]]])])],
    [("iterate",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "f", PVar "x"],
        ap [econst consCfun, evar "x", ap [evar "iterate", evar "f", ap [evar "f", evar "x"]]])])],
    [("numericEnumFromThen",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "n", PVar "m"],
        ap [evar "iterate", ap [econst plusMfun, ap [econst minusMfun, evar "m", evar "n"]], evar "n"])])],
    [("takeWhile",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "p", PCon nilCfun []],
        econst nilCfun),
       ([PVar "p", PCon consCfun [PVar "x", PVar "xs"]],
        eguarded [(ap [evar "p", evar "x"],
                   ap [econst consCfun, evar "x", ap [evar "takeWhile", evar "p", evar "xs"]]),
                  (evar "otherwise",
                   econst nilCfun)])])],
    [("numericEnumFromTo",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "n", PVar "m"],
        ap [evar "takeWhile", ap [evar "flip", econst leMfun, evar "m"], ap [evar "numericEnumFrom", evar "n"]])])],
    [("numericEnumFromThenTo",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "n", PVar "n'", PVar "m"],
        elet [[("p",
                Nothing,
                [([],
                  eguarded [(ap [econst geMfun, evar "n'", evar "n"],
                             ap [evar "flip", econst leMfun, evar "m"]),
                            (evar "otherwise",
                             ap [evar "flip", econst geMfun, evar "m"])])])]]
             (ap [evar "takeWhile", evar "p", ap [evar "numericEnumFromThen", evar "n", evar "n'"]]))])],
    [("reduce",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0)))),
      [([PVar "x", PVar "y"],
        elet [[("d",
                Nothing,
                [([],
                  ap [evar "gcd", evar "x", evar "y"])])]]
             (eguarded [(ap [econst eqMfun, evar "y", elit (LitInt 0)],
                         ap [evar "error", elit (LitStr "Ratio.%: zero denominator")]),
                        (evar "otherwise",
                         ap [econst consratCfun, ap [econst quotMfun, evar "x", evar "d"], ap [econst quotMfun, evar "y", evar "d"]])]))])],
    [("%",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0)))),
      [([PVar "x", PVar "y"],
        ap [evar "reduce", ap [econst timesMfun, evar "x", ap [econst signumMfun, evar "y"]], ap [econst absMfun, evar "y"]])])],
    [("realFloatToRational",
      Nothing,
      [([PVar "x"],
        elet [[("v236",
                Nothing,
                [([], ap [econst decodeFloatMfun, evar "x"])]),
               ("m",
                Nothing,
                [([], ecase (evar "v236") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "m")])]),
               ("n",
                Nothing,
                [([], ecase (evar "v236") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "n")])])],
              [("b",
                Nothing,
                [([],
                  ap [econst floatRadixMfun, evar "x"])])]]
             (ap [econst timesMfun, ap [evar "%", evar "m", elit (LitInt 1)], ap [evar "^^", ap [evar "%", evar "b", elit (LitInt 1)], evar "n"]]))])],
    [("floatToRational",
      Just (Forall []
             ([] :=>
              (tFloat `fn` tRational))),
      [([PVar "x"],
        ap [evar "realFloatToRational", evar "x"])])],
    [("doubleToRational",
      Just (Forall []
             ([] :=>
              (tDouble `fn` tRational))),
      [([PVar "x"],
        ap [evar "realFloatToRational", evar "x"])])],
    [("const",
      Just (Forall [Star, Star]
             ([] :=>
              (TGen 0 `fn` TGen 1 `fn` TGen 0))),
      [([PVar "k", PWildcard],
        evar "k")])],
    [("asTypeOf",
      Just (Forall [Star]
             ([] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0))),
      [([],
        evar "const")])],
    [("numerator",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TAp tRatio (TGen 0) `fn` TGen 0))),
      [([PCon consratCfun [PVar "x", PVar "y"]],
        evar "x")])],
    [("denominator",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TAp tRatio (TGen 0) `fn` TGen 0))),
      [([PCon consratCfun [PVar "x", PVar "y"]],
        evar "y")])],
    [("rationalToRealFloat",
      Nothing,
      [([PVar "x"],
        elet [[("x'",
                Nothing,
                [([],
                  ap [evar "f", evar "e"])]),
               ("f",
                Nothing,
                [([PVar "e"],
                  elet [[("y",
                          Nothing,
                          [([],
                            ap [econst encodeFloatMfun, ap [econst roundMfun, ap [econst timesMfun, evar "x", ap [evar "^^", ap [evar "%", elit (LitInt 1), evar "b"], evar "e"]]], evar "e"])])],
                        [("v237",
                          Nothing,
                          [([], ap [econst decodeFloatMfun, evar "y"])]),
                         ("e'",
                          Nothing,
                          [([], ecase (evar "v237") [(PCon tup2Cfun [PWildcard, PVar "e'"], evar "e'")])])]]
                       (eif (ap [econst eqMfun, evar "e'", evar "e"])
                            (evar "y")
                            (ap [evar "f", evar "e'"])))]),
               ("b",
                Nothing,
                [([],
                  ap [econst floatRadixMfun, evar "x'"])]),
               ("v238",
                Nothing,
                [([], ap [econst decodeFloatMfun, ap [econst divideMfun, ap [evar "asTypeOf", ap [econst fromIntegerMfun, ap [evar "numerator", evar "x"]], evar "x'"], ap [econst fromIntegerMfun, ap [evar "denominator", evar "x"]]]])]),
               ("e",
                Nothing,
                [([], ecase (evar "v238") [(PCon tup2Cfun [PWildcard, PVar "e"], evar "e")])])]]
             (evar "x'"))])],
    [("rationalToFloat",
      Just (Forall []
             ([] :=>
              (tRational `fn` tFloat))),
      [([],
        evar "rationalToRealFloat")])],
    [("rationalToDouble",
      Just (Forall []
             ([] :=>
              (tRational `fn` tDouble))),
      [([],
        evar "rationalToRealFloat")])],
    [("floatProperFraction",
      Nothing,
      [([PVar "x"],
        elet [[("v239",
                Nothing,
                [([], ap [econst decodeFloatMfun, evar "x"])]),
               ("m",
                Nothing,
                [([], ecase (evar "v239") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "m")])]),
               ("n",
                Nothing,
                [([], ecase (evar "v239") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "n")])])],
              [("b",
                Nothing,
                [([],
                  ap [econst floatRadixMfun, evar "x"])])],
              [("v240",
                Nothing,
                [([], ap [econst quotRemMfun, evar "m", ap [evar "^", evar "b", ap [econst negateMfun, evar "n"]]])]),
               ("w",
                Nothing,
                [([], ecase (evar "v240") [(PCon tup2Cfun [PVar "w", PVar "r"], evar "w")])]),
               ("r",
                Nothing,
                [([], ecase (evar "v240") [(PCon tup2Cfun [PVar "w", PVar "r"], evar "r")])])]]
             (eguarded [(ap [econst geMfun, evar "n", elit (LitInt 0)],
                         ap [econst tup2Cfun, ap [econst timesMfun, ap [econst fromIntegerMfun, evar "m"], ap [evar "^", ap [econst fromIntegerMfun, evar "b"], evar "n"]], elit (LitInt 0)]),
                        (evar "otherwise",
                         ap [econst tup2Cfun, ap [econst fromIntegerMfun, evar "w"], ap [econst encodeFloatMfun, evar "r", evar "n"]])]))])],
    [("fst",
      Just (Forall [Star, Star]
             ([] :=>
              (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 0))),
      [([PCon tup2Cfun [PVar "x", PWildcard]],
        evar "x")])],
    [("snd",
      Just (Forall [Star, Star]
             ([] :=>
              (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 1))),
      [([PCon tup2Cfun [PWildcard, PVar "y"]],
        evar "y")])],
    [("curry",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 2) `fn` TGen 0 `fn` TGen 1 `fn` TGen 2))),
      [([PVar "f", PVar "x", PVar "y"],
        ap [evar "f", ap [econst tup2Cfun, evar "x", evar "y"]])])],
    [("uncurry",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 2))),
      [([PVar "f", PVar "p"],
        ap [evar "f", ap [evar "fst", evar "p"], ap [evar "snd", evar "p"]])])],
    [("id",
      Just (Forall [Star]
             ([] :=>
              (TGen 0 `fn` TGen 0))),
      [([PVar "x"],
        evar "x")])],
    [("$",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1) `fn` TGen 0 `fn` TGen 1))),
      [([PVar "f", PVar "x"],
        ap [evar "f", evar "x"])])],
    [("until",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` (TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` TGen 0))),
      [([PVar "p", PVar "f", PVar "x"],
        eif (ap [evar "p", evar "x"])
            (evar "x")
            (ap [evar "until", evar "p", evar "f", ap [evar "f", evar "x"]]))])],
    [("undefined",
      Just (Forall [Star]
             ([] :=>
              (TGen 0))),
      [([],
        eguarded [(econst falseCfun,
                   evar "undefined")])])],
    [("intToRatio",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (tInt `fn` TAp tRatio (TGen 0)))),
      [([PVar "x"],
        ap [econst consratCfun, ap [econst fromIntMfun, evar "x"], elit (LitInt 1)])])],
    [("doubleToRatio",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (tDouble `fn` TAp tRatio (TGen 0)))),
      [([PVar "x"],
        elet [[("v241",
                Nothing,
                [([], ap [econst decodeFloatMfun, evar "x"])]),
               ("m",
                Nothing,
                [([], ecase (evar "v241") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "m")])]),
               ("n",
                Nothing,
                [([], ecase (evar "v241") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "n")])])],
              [("b",
                Nothing,
                [([],
                  ap [econst floatRadixMfun, evar "x"])])]]
             (eguarded [(ap [econst geMfun, evar "n", elit (LitInt 0)],
                         ap [evar "%", ap [econst timesMfun, ap [econst fromIntegerMfun, evar "m"], ap [evar "^", ap [econst fromIntegerMfun, evar "b"], evar "n"]], elit (LitInt 1)]),
                        (evar "otherwise",
                         ap [evar "%", ap [econst fromIntegerMfun, evar "m"], ap [evar "^", ap [econst fromIntegerMfun, evar "b"], ap [econst negateMfun, evar "n"]]])]))])],
    [("approxRational",
      Just (Forall [Star]
             ([isIn1 cRealFrac (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` tRational))),
      [([PVar "x", PVar "eps"],
        elet [[("simplest'",
                Nothing,
                [([PVar "n", PVar "d", PVar "n'", PVar "d'"],
                  elet [[("v242",
                          Nothing,
                          [([], ap [econst quotRemMfun, evar "n", evar "d"])]),
                         ("q",
                          Nothing,
                          [([], ecase (evar "v242") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
                         ("r",
                          Nothing,
                          [([], ecase (evar "v242") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])],
                        [("v243",
                          Nothing,
                          [([], ap [econst quotRemMfun, evar "n'", evar "d'"])]),
                         ("q'",
                          Nothing,
                          [([], ecase (evar "v243") [(PCon tup2Cfun [PVar "q'", PVar "r'"], evar "q'")])]),
                         ("r'",
                          Nothing,
                          [([], ecase (evar "v243") [(PCon tup2Cfun [PVar "q'", PVar "r'"], evar "r'")])])],
                        [("v244",
                          Nothing,
                          [([], ap [evar "simplest'", evar "d'", evar "r'", evar "d", evar "r"])]),
                         ("n''",
                          Nothing,
                          [([], ecase (evar "v244") [(PCon consratCfun [PVar "n''", PVar "d''"], evar "n''")])]),
                         ("d''",
                          Nothing,
                          [([], ecase (evar "v244") [(PCon consratCfun [PVar "n''", PVar "d''"], evar "d''")])])]]
                       (eguarded [(ap [econst eqMfun, evar "r", elit (LitInt 0)],
                                   ap [econst consratCfun, evar "q", elit (LitInt 1)]),
                                  (ap [econst neqMfun, evar "q", evar "q'"],
                                   ap [econst consratCfun, ap [econst plusMfun, evar "q", elit (LitInt 1)], elit (LitInt 1)]),
                                  (evar "otherwise",
                                   ap [econst consratCfun, ap [econst plusMfun, ap [econst timesMfun, evar "q", evar "n''"], evar "d''"], evar "n''"])]))])],
              [("simplest",
                Nothing,
                [([PVar "x", PVar "y"],
                  elet [[("v245",
                          Nothing,
                          [([], ap [econst toRationalMfun, evar "x"])]),
                         ("xr",
                          Nothing,
                          [([], ecase (evar "v245") [(PAs "xr" (PCon consratCfun [PVar "n", PVar "d"]), evar "xr")])]),
                         ("n",
                          Nothing,
                          [([], ecase (evar "v245") [(PAs "xr" (PCon consratCfun [PVar "n", PVar "d"]), evar "n")])]),
                         ("d",
                          Nothing,
                          [([], ecase (evar "v245") [(PAs "xr" (PCon consratCfun [PVar "n", PVar "d"]), evar "d")])])],
                        [("v246",
                          Nothing,
                          [([], ap [econst toRationalMfun, evar "y"])]),
                         ("n'",
                          Nothing,
                          [([], ecase (evar "v246") [(PCon consratCfun [PVar "n'", PVar "d'"], evar "n'")])]),
                         ("d'",
                          Nothing,
                          [([], ecase (evar "v246") [(PCon consratCfun [PVar "n'", PVar "d'"], evar "d'")])])]]
                       (eguarded [(ap [econst ltMfun, evar "y", evar "x"],
                                   ap [evar "simplest", evar "y", evar "x"]),
                                  (ap [econst eqMfun, evar "x", evar "y"],
                                   evar "xr"),
                                  (ap [econst gtMfun, evar "x", elit (LitInt 0)],
                                   ap [evar "simplest'", evar "n", evar "d", evar "n'", evar "d'"]),
                                  (ap [econst ltMfun, evar "y", elit (LitInt 0)],
                                   ap [econst negateMfun, ap [evar "simplest'", ap [econst negateMfun, evar "n'"], evar "d'", ap [econst negateMfun, evar "n"], evar "d"]]),
                                  (evar "otherwise",
                                   ap [econst consratCfun, elit (LitInt 0), elit (LitInt 1)])]))])]]
             (ap [evar "simplest", ap [econst minusMfun, evar "x", evar "eps"], ap [econst plusMfun, evar "x", evar "eps"]]))])],
    [("head",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([PCon consCfun [PVar "x", PWildcard]],
        evar "x")])],
    [("last",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([pCons (PVar "x") pNil],
        evar "x"),
       ([PCon consCfun [PWildcard, PVar "xs"]],
        ap [evar "last", evar "xs"])])],
    [("tail",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PCon consCfun [PWildcard, PVar "xs"]],
        evar "xs")])],
    [("init",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([pCons (PVar "x") pNil],
        econst nilCfun),
       ([PCon consCfun [PVar "x", PVar "xs"]],
        ap [econst consCfun, evar "x", ap [evar "init", evar "xs"]])])],
    [("null",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` tBool))),
      [([PCon nilCfun []],
        econst trueCfun),
       ([PCon consCfun [PWildcard, PWildcard]],
        econst falseCfun)])],
    [("++",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PCon nilCfun [], PVar "ys"],
        evar "ys"),
       ([PCon consCfun [PVar "x", PVar "xs"], PVar "ys"],
        ap [econst consCfun, evar "x", ap [evar "++", evar "xs", evar "ys"]])])],
    [("filter",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "p", PVar "xs"],
        eCompFrom (PVar "x") (evar "xs")
        (eCompGuard (ap [evar "p", evar "x"])
         (eListRet (evar "x"))))])],
    [("concat",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TAp tList (TGen 0)) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "foldr", evar "++", econst nilCfun])])],
    [("foldl'",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TGen 0))),
      [([PVar "f", PVar "a", PCon nilCfun []],
        evar "a"),
       ([PVar "f", PVar "a", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "$!", ap [evar "foldl'", evar "f"], ap [evar "f", evar "a", evar "x"], evar "xs"])])],
    [("length",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` tInt))),
      [([],
        ap [evar "foldl'", elambda ([PVar "n", PWildcard],
                                    ap [econst plusMfun, evar "n", elit (LitInt 1)]), elit (LitInt 0)])])],
    [("!!",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` tInt `fn` TGen 0))),
      [([PCon consCfun [PVar "x", PWildcard], PLit (LitInt 0)],
        evar "x"),
       ([PCon consCfun [PWildcard, PVar "xs"], PVar "n"],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [evar "!!", evar "xs", ap [econst minusMfun, evar "n", elit (LitInt 1)]])]),
       ([PCon consCfun [PWildcard, PWildcard], PWildcard],
        ap [evar "error", elit (LitStr "Prelude.!!: negative index")]),
       ([PCon nilCfun [], PWildcard],
        ap [evar "error", elit (LitStr "Prelude.!!: index too large")])])],
    [("foldl",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TGen 0))),
      [([PVar "f", PVar "z", PCon nilCfun []],
        evar "z"),
       ([PVar "f", PVar "z", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "foldl", evar "f", ap [evar "f", evar "z", evar "x"], evar "xs"])])],
    [("foldl1",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0))),
      [([PVar "f", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "foldl", evar "f", evar "x", evar "xs"])])],
    [("scanl",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 0)))),
      [([PVar "f", PVar "q", PVar "xs"],
        ap [econst consCfun, evar "q", ecase (evar "xs")
                                             [(PCon nilCfun [],
                                               econst nilCfun),
                                              (PCon consCfun [PVar "x", PVar "xs"],
                                               ap [evar "scanl", evar "f", ap [evar "f", evar "q", evar "x"], evar "xs"])]])])],
    [("scanl1",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "f", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "scanl", evar "f", evar "x", evar "xs"])])],
    [("foldr1",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0))),
      [([PVar "f", pCons (PVar "x") pNil],
        evar "x"),
       ([PVar "f", PCon consCfun [PVar "x", PVar "xs"]],
        ap [evar "f", evar "x", ap [evar "foldr1", evar "f", evar "xs"]])])],
    [("scanr",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))),
      [([PVar "f", PVar "q0", PCon nilCfun []],
        eCons (evar "q0")
        eNil),
       ([PVar "f", PVar "q0", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v247",
                Nothing,
                [([], ap [evar "scanr", evar "f", evar "q0", evar "xs"])]),
               ("qs",
                Nothing,
                [([], ecase (evar "v247") [(PAs "qs" (PCon consCfun [PVar "q", PWildcard]), evar "qs")])]),
               ("q",
                Nothing,
                [([], ecase (evar "v247") [(PAs "qs" (PCon consCfun [PVar "q", PWildcard]), evar "q")])])]]
             (ap [econst consCfun, ap [evar "f", evar "x", evar "q"], evar "qs"]))])],
    [("scanr1",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "f", pCons (PVar "x") pNil],
        eCons (evar "x")
        eNil),
       ([PVar "f", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v248",
                Nothing,
                [([], ap [evar "scanr1", evar "f", evar "xs"])]),
               ("qs",
                Nothing,
                [([], ecase (evar "v248") [(PAs "qs" (PCon consCfun [PVar "q", PWildcard]), evar "qs")])]),
               ("q",
                Nothing,
                [([], ecase (evar "v248") [(PAs "qs" (PCon consCfun [PVar "q", PWildcard]), evar "q")])])]]
             (ap [econst consCfun, ap [evar "f", evar "x", evar "q"], evar "qs"]))])],
    [("repeat",
      Just (Forall [Star]
             ([] :=>
              (TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "x"],
        elet [[("xs",
                Nothing,
                [([],
                  ap [econst consCfun, evar "x", evar "xs"])])]]
             (evar "xs"))])],
    [("take",
      Just (Forall [Star]
             ([] :=>
              (tInt `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PLit (LitInt 0), PWildcard],
        econst nilCfun),
       ([PWildcard, PCon nilCfun []],
        econst nilCfun),
       ([PVar "n", PCon consCfun [PVar "x", PVar "xs"]],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [econst consCfun, evar "x", ap [evar "take", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"]])]),
       ([PWildcard, PWildcard],
        ap [evar "error", elit (LitStr "Prelude.take: negative argument")])])],
    [("replicate",
      Just (Forall [Star]
             ([] :=>
              (tInt `fn` TGen 0 `fn` TAp tList (TGen 0)))),
      [([PVar "n", PVar "x"],
        ap [evar "take", evar "n", ap [evar "repeat", evar "x"]])])],
    [("cycle",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PCon nilCfun []],
        ap [evar "error", elit (LitStr "Prelude.cycle: empty list")]),
       ([PVar "xs"],
        elet [[("xs'",
                Nothing,
                [([],
                  ap [evar "++", evar "xs", evar "xs'"])])]]
             (evar "xs'"))])],
    [("drop",
      Just (Forall [Star]
             ([] :=>
              (tInt `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PLit (LitInt 0), PVar "xs"],
        evar "xs"),
       ([PWildcard, PCon nilCfun []],
        econst nilCfun),
       ([PVar "n", PCon consCfun [PWildcard, PVar "xs"]],
        eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                   ap [evar "drop", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"])]),
       ([PWildcard, PWildcard],
        ap [evar "error", elit (LitStr "Prelude.drop: negative argument")])])],
    [("splitAt",
      Just (Forall [Star]
             ([] :=>
              (tInt `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0))))),
      [([PLit (LitInt 0), PVar "xs"],
        ap [econst tup2Cfun, econst nilCfun, evar "xs"]),
       ([PWildcard, PCon nilCfun []],
        ap [econst tup2Cfun, econst nilCfun, econst nilCfun]),
       ([PVar "n", PCon consCfun [PVar "x", PVar "xs"]],
        elet [[("v249",
                Nothing,
                [([], ap [evar "splitAt", ap [econst minusMfun, evar "n", elit (LitInt 1)], evar "xs"])]),
               ("xs'",
                Nothing,
                [([], ecase (evar "v249") [(PCon tup2Cfun [PVar "xs'", PVar "xs''"], evar "xs'")])]),
               ("xs''",
                Nothing,
                [([], ecase (evar "v249") [(PCon tup2Cfun [PVar "xs'", PVar "xs''"], evar "xs''")])])]]
             (eguarded [(ap [econst gtMfun, evar "n", elit (LitInt 0)],
                         ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "xs'"], evar "xs''"])])),
       ([PWildcard, PWildcard],
        ap [evar "error", elit (LitStr "Prelude.splitAt: negative argument")])])],
    [("dropWhile",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([PVar "p", PCon nilCfun []],
        econst nilCfun),
       ([PVar "p", PAs "xs" (PCon consCfun [PVar "x", PVar "xs'"])],
        eguarded [(ap [evar "p", evar "x"],
                   ap [evar "dropWhile", evar "p", evar "xs'"]),
                  (evar "otherwise",
                   evar "xs")])])],
    [("span",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0))))),
      [([PVar "p", PCon nilCfun []],
        ap [econst tup2Cfun, econst nilCfun, econst nilCfun]),
       ([PVar "p", PAs "xs" (PCon consCfun [PVar "x", PVar "xs'"])],
        elet [[("v250",
                Nothing,
                [([], ap [evar "span", evar "p", evar "xs'"])]),
               ("ys",
                Nothing,
                [([], ecase (evar "v250") [(PCon tup2Cfun [PVar "ys", PVar "zs"], evar "ys")])]),
               ("zs",
                Nothing,
                [([], ecase (evar "v250") [(PCon tup2Cfun [PVar "ys", PVar "zs"], evar "zs")])])]]
             (eguarded [(ap [evar "p", evar "x"],
                         ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "ys"], evar "zs"]),
                        (evar "otherwise",
                         ap [econst tup2Cfun, econst nilCfun, evar "xs"])]))])],
    [("break",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0))))),
      [([PVar "p"],
        ap [evar "span", ap [evar ".", evar "not", evar "p"]])])],
    [("lines",
      Just (Forall []
             ([] :=>
              (tString `fn` TAp tList tString))),
      [([PLit (LitStr "")],
        econst nilCfun),
       ([PVar "s"],
        elet [[("v251",
                Nothing,
                [([], ap [evar "break", ap [econst eqMfun, elit (LitChar '\n')], evar "s"])]),
               ("l",
                Nothing,
                [([], ecase (evar "v251") [(PCon tup2Cfun [PVar "l", PVar "s'"], evar "l")])]),
               ("s'",
                Nothing,
                [([], ecase (evar "v251") [(PCon tup2Cfun [PVar "l", PVar "s'"], evar "s'")])])]]
             (ap [econst consCfun, evar "l", ecase (evar "s'")
                                                   [(PCon nilCfun [],
                                                     econst nilCfun),
                                                    (PCon consCfun [PWildcard, PVar "s''"],
                                                     ap [evar "lines", evar "s''"])]]))])],
    [("words",
      Just (Forall []
             ([] :=>
              (tString `fn` TAp tList tString))),
      [([PVar "s"],
        ecase (ap [evar "dropWhile", evar "isSpace", evar "s"])
              [(PLit (LitStr ""),
                econst nilCfun),
               (PVar "s'",
                elet [[("v252",
                        Nothing,
                        [([], ap [evar "break", evar "isSpace", evar "s'"])]),
                       ("w",
                        Nothing,
                        [([], ecase (evar "v252") [(PCon tup2Cfun [PVar "w", PVar "s''"], evar "w")])]),
                       ("s''",
                        Nothing,
                        [([], ecase (evar "v252") [(PCon tup2Cfun [PVar "w", PVar "s''"], evar "s''")])])]]
                     (ap [econst consCfun, evar "w", ap [evar "words", evar "s''"]]))])])],
    [("concatMap",
      Just (Forall [Star, Star]
             ([] :=>
              ((TGen 0 `fn` TAp tList (TGen 1)) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))),
      [([PVar "f"],
        ap [evar ".", evar "concat", ap [evar "map", evar "f"]])])],
    [("unlines",
      Just (Forall []
             ([] :=>
              (TAp tList tString `fn` tString))),
      [([],
        ap [evar "concatMap", elambda ([PVar "l"],
                                       ap [evar "++", evar "l", elit (LitStr "\n")])])])],
    [("unwords",
      Just (Forall []
             ([] :=>
              (TAp tList tString `fn` tString))),
      [([PCon nilCfun []],
        econst nilCfun),
       ([PVar "ws"],
        ap [evar "foldr1", elambda ([PVar "w", PVar "s"],
                                    ap [evar "++", evar "w", ap [econst consCfun, elit (LitChar ' '), evar "s"]]), evar "ws"])])],
    [("reverse",
      Just (Forall [Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0)))),
      [([],
        ap [evar "foldl", ap [evar "flip", econst consCfun], econst nilCfun])])],
    [("and",
      Just (Forall []
             ([] :=>
              (TAp tList tBool `fn` tBool))),
      [([],
        ap [evar "foldr", evar "&&", econst trueCfun])])],
    [("or",
      Just (Forall []
             ([] :=>
              (TAp tList tBool `fn` tBool))),
      [([],
        ap [evar "foldr", evar "||", econst falseCfun])])],
    [("any",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` tBool))),
      [([PVar "p"],
        ap [evar ".", evar "or", ap [evar "map", evar "p"]])])],
    [("all",
      Just (Forall [Star]
             ([] :=>
              ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` tBool))),
      [([PVar "p"],
        ap [evar ".", evar "and", ap [evar "map", evar "p"]])])],
    [("elem",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=>
              (TGen 0 `fn` TAp tList (TGen 0) `fn` tBool))),
      [([],
        ap [evar ".", evar "any", econst eqMfun])])],
    [("notElem",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=>
              (TGen 0 `fn` TAp tList (TGen 0) `fn` tBool))),
      [([],
        ap [evar ".", evar "all", econst neqMfun])])],
    [("lookup",
      Just (Forall [Star, Star]
             ([isIn1 cEq (TGen 0)] :=>
              (TGen 0 `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp tMaybe (TGen 1)))),
      [([PVar "k", PCon nilCfun []],
        econst nothingCfun),
       ([PVar "k", PCon consCfun [PCon tup2Cfun [PVar "x", PVar "y"], PVar "xys"]],
        eguarded [(ap [econst eqMfun, evar "k", evar "x"],
                   ap [econst justCfun, evar "y"]),
                  (evar "otherwise",
                   ap [evar "lookup", evar "k", evar "xys"])])])],
    [("sum",
      Just (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([],
        ap [evar "foldl'", econst plusMfun, elit (LitInt 0)])])],
    [("product",
      Just (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([],
        ap [evar "foldl'", econst timesMfun, elit (LitInt 1)])])],
    [("maximum",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([],
        ap [evar "foldl1", econst maxMfun])])],
    [("minimum",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=>
              (TAp tList (TGen 0) `fn` TGen 0))),
      [([],
        ap [evar "foldl1", econst minMfun])])],
    [("zipWith",
      Just (Forall [Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b"], ap [evar "zipWith", evar "z", evar "as", evar "bs"]]),
       ([PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip",
      Just (Forall [Star, Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1))))),
      [([],
        ap [evar "zipWith", elambda ([PVar "a", PVar "b"],
                                     ap [econst tup2Cfun, evar "a", evar "b"])])])],
    [("zipWith3",
      Just (Forall [Star, Star, Star, Star]
             ([] :=>
              ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3)))),
      [([PVar "z", PCon consCfun [PVar "a", PVar "as"], PCon consCfun [PVar "b", PVar "bs"], PCon consCfun [PVar "c", PVar "cs"]],
        ap [econst consCfun, ap [evar "z", evar "a", evar "b", evar "c"], ap [evar "zipWith3", evar "z", evar "as", evar "bs", evar "cs"]]),
       ([PWildcard, PWildcard, PWildcard, PWildcard],
        econst nilCfun)])],
    [("zip3",
      Just (Forall [Star, Star, Star]
             ([] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))))),
      [([],
        ap [evar "zipWith3", elambda ([PVar "a", PVar "b", PVar "c"],
                                      ap [econst tup3Cfun, evar "a", evar "b", evar "c"])])])],
    [("unzip",
      Just (Forall [Star, Star]
             ([] :=>
              (TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 1))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup2Cfun [PVar "a", PVar "b"], PLazy (PCon tup2Cfun [PVar "as", PVar "bs"])],
                                   ap [econst tup2Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"]]), ap [econst tup2Cfun, econst nilCfun, econst nilCfun]])])],
    [("unzip3",
      Just (Forall [Star, Star, Star]
             ([] :=>
              (TAp tList (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)) `fn` TAp (TAp (TAp tTuple3 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))))),
      [([],
        ap [evar "foldr", elambda ([PCon tup3Cfun [PVar "a", PVar "b", PVar "c"], PLazy (PCon tup3Cfun [PVar "as", PVar "bs", PVar "cs"])],
                                   ap [econst tup3Cfun, ap [econst consCfun, evar "a", evar "as"], ap [econst consCfun, evar "b", evar "bs"], ap [econst consCfun, evar "c", evar "cs"]]), ap [econst tup3Cfun, econst nilCfun, econst nilCfun, econst nilCfun]])])],
    [("reads",
      Just (Forall [Star]
             ([isIn1 cRead (TGen 0)] :=>
              tReadS (TGen 0))),
      [([],
        ap [econst readsPrecMfun, elit (LitInt 0)])])],
    [("shows",
      Just (Forall [Star]
             ([isIn1 cShow (TGen 0)] :=>
              (TGen 0 `fn` tShowS))),
      [([],
        ap [econst showsPrecMfun, elit (LitInt 0)])])],
    [("nonnull",
      Just (Forall []
             ([] :=>
              ((tChar `fn` tBool) `fn` tReadS tString))),
      [([PVar "p", PVar "s"],
        eCompFrom (PCon tup2Cfun [PAs "cs" (PCon consCfun [PWildcard, PWildcard]), PVar "t"]) (eCons (ap [evar "span", evar "p", evar "s"])
                                                                                              eNil)
        (eListRet (ap [econst tup2Cfun, evar "cs", evar "t"])))])],
    [("lexDigits",
      Just (Forall []
             ([] :=>
              tReadS tString)),
      [([],
        ap [evar "nonnull", evar "isDigit"])])],
    [("lexmatch",
      Just (Forall [Star]
             ([isIn1 cEq (TGen 0)] :=>
              (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0))))),
      [([PCon consCfun [PVar "x", PVar "xs"], PCon consCfun [PVar "y", PVar "ys"]],
        eguarded [(ap [econst eqMfun, evar "x", evar "y"],
                   ap [evar "lexmatch", evar "xs", evar "ys"])]),
       ([PVar "xs", PVar "ys"],
        ap [econst tup2Cfun, evar "xs", evar "ys"])])],
    [("asciiTab",
      Nothing,
      [([],
        ap [evar "zip", ap [econst enumFromToMfun, elit (LitChar '\NUL'), elit (LitChar ' ')], eCons (elit (LitStr "NUL"))
                                                                                               (eCons (elit (LitStr "SOH"))
                                                                                               (eCons (elit (LitStr "STX"))
                                                                                               (eCons (elit (LitStr "ETX"))
                                                                                               (eCons (elit (LitStr "EOT"))
                                                                                               (eCons (elit (LitStr "ENQ"))
                                                                                               (eCons (elit (LitStr "ACK"))
                                                                                               (eCons (elit (LitStr "BEL"))
                                                                                               (eCons (elit (LitStr "BS"))
                                                                                               (eCons (elit (LitStr "HT"))
                                                                                               (eCons (elit (LitStr "LF"))
                                                                                               (eCons (elit (LitStr "VT"))
                                                                                               (eCons (elit (LitStr "FF"))
                                                                                               (eCons (elit (LitStr "CR"))
                                                                                               (eCons (elit (LitStr "SO"))
                                                                                               (eCons (elit (LitStr "SI"))
                                                                                               (eCons (elit (LitStr "DLE"))
                                                                                               (eCons (elit (LitStr "DC1"))
                                                                                               (eCons (elit (LitStr "DC2"))
                                                                                               (eCons (elit (LitStr "DC3"))
                                                                                               (eCons (elit (LitStr "DC4"))
                                                                                               (eCons (elit (LitStr "NAK"))
                                                                                               (eCons (elit (LitStr "SYN"))
                                                                                               (eCons (elit (LitStr "ETB"))
                                                                                               (eCons (elit (LitStr "CAN"))
                                                                                               (eCons (elit (LitStr "EM"))
                                                                                               (eCons (elit (LitStr "SUB"))
                                                                                               (eCons (elit (LitStr "ESC"))
                                                                                               (eCons (elit (LitStr "FS"))
                                                                                               (eCons (elit (LitStr "GS"))
                                                                                               (eCons (elit (LitStr "RS"))
                                                                                               (eCons (elit (LitStr "US"))
                                                                                               (eCons (elit (LitStr "SP"))
                                                                                               eNil))))))))))))))))))))))))))))))))])])],
    [("lexLitChar",
      Just (Forall []
             ([] :=>
              tReadS tString)),
      [([PCon consCfun [PLit (LitChar '\\'), PVar "s"]],
        elet [[("lexEsc",
                Nothing,
                [([PCon consCfun [PVar "c", PVar "s"]],
                  eguarded [(ap [evar "elem", evar "c", elit (LitStr "abfnrtv\\\\\"'")],
                             eCons (ap [econst tup2Cfun, eCons (evar "c")
                                                         eNil, evar "s"])
                             eNil)]),
                 ([PCon consCfun [PLit (LitChar '^'), PCon consCfun [PVar "c", PVar "s"]]],
                  eguarded [(ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar '@')], ap [econst leMfun, evar "c", elit (LitChar '_')]],
                             eCons (ap [econst tup2Cfun, eCons (elit (LitChar '^'))
                                                         (eCons (evar "c")
                                                         eNil), evar "s"])
                             eNil)]),
                 ([PAs "s" (PCon consCfun [PVar "d", PWildcard])],
                  eguarded [(ap [evar "isDigit", evar "d"],
                             ap [evar "lexDigits", evar "s"])]),
                 ([PAs "s" (PCon consCfun [PVar "c", PWildcard])],
                  eguarded [(ap [evar "isUpper", evar "c"],
                             elet [[("table",
                                     Nothing,
                                     [([],
                                       ap [econst consCfun, ap [econst tup2Cfun, elit (LitChar '\DEL'), elit (LitStr "DEL")], evar "asciiTab"])])]]
                                  (ecase (eCompFrom (PCon tup2Cfun [PVar "c", PVar "mne"]) (evar "table")
                                          (eCompFrom (PCon tup2Cfun [PCon nilCfun [], PVar "s'"]) (eCons (ap [evar "lexmatch", evar "mne", evar "s"])
                                                                                                  eNil)
                                           (eListRet (ap [econst tup2Cfun, evar "mne", evar "s'"]))))
                                         [(PCon consCfun [PVar "pr", PWildcard],
                                           eCons (evar "pr")
                                           eNil),
                                          (PCon nilCfun [],
                                           econst nilCfun)]))]),
                 ([PWildcard],
                  econst nilCfun)])]]
             (eCompFrom (PCon tup2Cfun [PVar "esc", PVar "t"]) (ap [evar "lexEsc", evar "s"])
              (eListRet (ap [econst tup2Cfun, ap [econst consCfun, elit (LitChar '\\'), evar "esc"], evar "t"])))),
       ([PCon consCfun [PVar "c", PVar "s"]],
        eCons (ap [econst tup2Cfun, eCons (evar "c")
                                    eNil, evar "s"])
        eNil),
       ([PLit (LitStr "")],
        econst nilCfun)])],
    [("lex",
      Just (Forall []
             ([] :=>
              tReadS tString)),
      [([PLit (LitStr "")],
        eCons (ap [econst tup2Cfun, elit (LitStr ""), elit (LitStr "")])
        eNil),
       ([PCon consCfun [PVar "c", PVar "s"]],
        eguarded [(ap [evar "isSpace", evar "c"],
                   ap [evar "lex", ap [evar "dropWhile", evar "isSpace", evar "s"]])]),
       ([PCon consCfun [PLit (LitChar '\''), PVar "s"]],
        eCompFrom (PCon tup2Cfun [PVar "ch", PCon consCfun [PLit (LitChar '\''), PVar "t"]]) (ap [evar "lexLitChar", evar "s"])
        (eCompGuard (ap [econst neqMfun, evar "ch", elit (LitStr "'")])
         (eListRet (ap [econst tup2Cfun, ap [econst consCfun, elit (LitChar '\''), ap [evar "++", evar "ch", elit (LitStr "'")]], evar "t"])))),
       ([PCon consCfun [PLit (LitChar '"'), PVar "s"]],
        elet [[("lexStrItem",
                Nothing,
                [([PCon consCfun [PLit (LitChar '\\'), PCon consCfun [PLit (LitChar '&'), PVar "s"]]],
                  eCons (ap [econst tup2Cfun, elit (LitStr "\\\\&"), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar '\\'), PCon consCfun [PVar "c", PVar "s"]]],
                  eguarded [(ap [evar "isSpace", evar "c"],
                             eCompFrom (PCon consCfun [PLit (LitChar '\\'), PVar "t"]) (eCons (ap [evar "dropWhile", evar "isSpace", evar "s"])
                                                                                       eNil)
                             (eListRet (ap [econst tup2Cfun, elit (LitStr ""), evar "t"])))]),
                 ([PVar "s"],
                  ap [evar "lexLitChar", evar "s"])])],
              [("lexString",
                Nothing,
                [([PCon consCfun [PLit (LitChar '"'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitStr "\""), evar "s"])
                  eNil),
                 ([PVar "s"],
                  eCompFrom (PCon tup2Cfun [PVar "ch", PVar "t"]) (ap [evar "lexStrItem", evar "s"])
                  (eCompFrom (PCon tup2Cfun [PVar "str", PVar "u"]) (ap [evar "lexString", evar "t"])
                   (eListRet (ap [econst tup2Cfun, ap [evar "++", evar "ch", evar "str"], evar "u"]))))])]]
             (eCompFrom (PCon tup2Cfun [PVar "str", PVar "t"]) (ap [evar "lexString", evar "s"])
              (eListRet (ap [econst tup2Cfun, ap [econst consCfun, elit (LitChar '"'), evar "str"], evar "t"])))),
       ([PCon consCfun [PVar "c", PVar "s"]],
        elet [[("isSingle",
                Nothing,
                [([PVar "c"],
                  ap [evar "elem", evar "c", elit (LitStr ",;()[]{}_`")])])],
              [("isSym",
                Nothing,
                [([PVar "c"],
                  ap [evar "elem", evar "c", elit (LitStr "!@#$%&*+./<=>?\\\\^|:-~")])])],
              [("isIdChar",
                Nothing,
                [([PVar "c"],
                  ap [evar "||", ap [evar "isAlphaNum", evar "c"], ap [evar "elem", evar "c", elit (LitStr "_'")]])])],
              [("lexExp",
                Nothing,
                [([PCon consCfun [PVar "e", PVar "s"]],
                  eguarded [(ap [evar "elem", evar "e", elit (LitStr "eE")],
                             ap [evar "++", eCompFrom (PCon consCfun [PVar "c", PVar "t"]) (eCons (evar "s")
                                                                                           eNil)
                                            (eCompGuard (ap [evar "elem", evar "c", elit (LitStr "+-")])
                                             (eCompFrom (PCon tup2Cfun [PVar "ds", PVar "u"]) (ap [evar "lexDigits", evar "t"])
                                              (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "e", ap [econst consCfun, evar "c", evar "ds"]], evar "u"])))), eCompFrom (PCon tup2Cfun [PVar "ds", PVar "t"]) (ap [evar "lexDigits", evar "s"])
                                                                                                                                                                        (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "e", evar "ds"], evar "t"]))])]),
                 ([PVar "s"],
                  eCons (ap [econst tup2Cfun, elit (LitStr ""), evar "s"])
                  eNil)])],
              [("lexFracExp",
                Nothing,
                [([PCon consCfun [PLit (LitChar '.'), PVar "s"]],
                  eCompFrom (PCon tup2Cfun [PVar "ds", PVar "t"]) (ap [evar "lexDigits", evar "s"])
                  (eCompFrom (PCon tup2Cfun [PVar "e", PVar "u"]) (ap [evar "lexExp", evar "t"])
                   (eListRet (ap [econst tup2Cfun, ap [econst consCfun, elit (LitChar '.'), ap [evar "++", evar "ds", evar "e"]], evar "u"])))),
                 ([PVar "s"],
                  eCons (ap [econst tup2Cfun, elit (LitStr ""), evar "s"])
                  eNil)])]]
             (eguarded [(ap [evar "isSingle", evar "c"],
                         eCons (ap [econst tup2Cfun, eCons (evar "c")
                                                     eNil, evar "s"])
                         eNil),
                        (ap [evar "isSym", evar "c"],
                         eCompFrom (PCon tup2Cfun [PVar "sym", PVar "t"]) (eCons (ap [evar "span", evar "isSym", evar "s"])
                                                                          eNil)
                         (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "c", evar "sym"], evar "t"]))),
                        (ap [evar "isAlpha", evar "c"],
                         eCompFrom (PCon tup2Cfun [PVar "nam", PVar "t"]) (eCons (ap [evar "span", evar "isIdChar", evar "s"])
                                                                          eNil)
                         (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "c", evar "nam"], evar "t"]))),
                        (ap [evar "isDigit", evar "c"],
                         eCompFrom (PCon tup2Cfun [PVar "ds", PVar "s"]) (eCons (ap [evar "span", evar "isDigit", evar "s"])
                                                                         eNil)
                         (eCompFrom (PCon tup2Cfun [PVar "fe", PVar "t"]) (ap [evar "lexFracExp", evar "s"])
                          (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "c", ap [evar "++", evar "ds", evar "fe"]], evar "t"])))),
                        (evar "otherwise",
                         econst nilCfun)]))])],
    [("read",
      Just (Forall [Star]
             ([isIn1 cRead (TGen 0)] :=>
              (tString `fn` TGen 0))),
      [([PVar "s"],
        ecase (eCompFrom (PCon tup2Cfun [PVar "x", PVar "t"]) (ap [evar "reads", evar "s"])
               (eCompFrom (PCon tup2Cfun [PLit (LitStr ""), PLit (LitStr "")]) (ap [evar "lex", evar "t"])
                (eListRet (evar "x"))))
              [(pCons (PVar "x") pNil,
                evar "x"),
               (PCon nilCfun [],
                ap [evar "error", elit (LitStr "Prelude.read: no parse")]),
               (PWildcard,
                ap [evar "error", elit (LitStr "Prelude.read: ambiguous parse")])])])],
    [("showChar",
      Just (Forall []
             ([] :=>
              (tChar `fn` tShowS))),
      [([],
        econst consCfun)])],
    [("showString",
      Just (Forall []
             ([] :=>
              (tString `fn` tShowS))),
      [([],
        evar "++")])],
    [("showParen",
      Just (Forall []
             ([] :=>
              (tBool `fn` tShowS `fn` tShowS))),
      [([PVar "b", PVar "p"],
        eif (evar "b")
            (ap [evar ".", ap [evar "showChar", elit (LitChar '(')], ap [evar ".", evar "p", ap [evar "showChar", elit (LitChar ')')]]])
            (evar "p"))])],
    [("showField",
      Just (Forall [Star]
             ([isIn1 cShow (TGen 0)] :=>
              (tString `fn` TGen 0 `fn` tShowS))),
      [([PVar "m", PVar "v"],
        ap [evar ".", ap [evar "showString", evar "m"], ap [evar ".", ap [evar "showChar", elit (LitChar '=')], ap [evar "shows", evar "v"]]])])],
    [("readParen",
      Just (Forall [Star]
             ([] :=>
              (tBool `fn` tReadS (TGen 0) `fn` tReadS (TGen 0)))),
      [([PVar "b", PVar "g"],
        elet [[("optional",
                Nothing,
                [([PVar "r"],
                  ap [evar "++", ap [evar "g", evar "r"], ap [evar "mandatory", evar "r"]])]),
               ("mandatory",
                Nothing,
                [([PVar "r"],
                  eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "s"]) (ap [evar "lex", evar "r"])
                  (eCompFrom (PCon tup2Cfun [PVar "x", PVar "t"]) (ap [evar "optional", evar "s"])
                   (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "u"]) (ap [evar "lex", evar "t"])
                    (eListRet (ap [econst tup2Cfun, evar "x", evar "u"])))))])]]
             (eif (evar "b")
                  (evar "mandatory")
                  (evar "optional")))])],
    [("readField",
      Just (Forall [Star]
             ([isIn1 cRead (TGen 0)] :=>
              (tString `fn` tReadS (TGen 0)))),
      [([PVar "m", PVar "s0"],
        eCompFrom (PCon tup2Cfun [PVar "t", PVar "s1"]) (ap [evar "lex", evar "s0"])
        (eCompGuard (ap [econst eqMfun, evar "t", evar "m"])
         (eCompFrom (PCon tup2Cfun [PLit (LitStr "="), PVar "s2"]) (ap [evar "lex", evar "s1"])
          (eCompFrom (PVar "r") (ap [evar "reads", evar "s2"])
           (eListRet (evar "r"))))))])],
    [("isOctDigit",
      Nothing,
      [([PVar "c"],
        ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar '0')], ap [econst leMfun, evar "c", elit (LitChar '7')]])])],
    [("isHexDigit",
      Nothing,
      [([PVar "c"],
        ap [evar "||", ap [evar "isDigit", evar "c"], ap [evar "||", ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'A')], ap [econst leMfun, evar "c", elit (LitChar 'F')]], ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar 'a')], ap [econst leMfun, evar "c", elit (LitChar 'f')]]]])])],
    [("readInt",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` (tChar `fn` tBool) `fn` (tChar `fn` tInt) `fn` tReadS (TGen 0)))),
      [([PVar "radix", PVar "isDig", PVar "digToInt", PVar "s"],
        eCompFrom (PCon tup2Cfun [PVar "ds", PVar "r"]) (ap [evar "nonnull", evar "isDig", evar "s"])
        (eListRet (ap [econst tup2Cfun, ap [evar "foldl1", elambda ([PVar "n", PVar "d"],
                                                                    ap [econst plusMfun, ap [econst timesMfun, evar "n", evar "radix"], evar "d"]), ap [evar "map", ap [evar ".", evar "fromIntegral", evar "digToInt"], evar "ds"]], evar "r"])))])],
    [("readHex",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              tReadS (TGen 0))),
      [([],
        elet [[("hex",
                Nothing,
                [([PVar "d"],
                  ap [econst minusMfun, ap [econst fromEnumMfun, evar "d"], eif (ap [evar "isDigit", evar "d"])
                                                                                (ap [econst fromEnumMfun, elit (LitChar '0')])
                                                                                (ap [econst minusMfun, ap [econst fromEnumMfun, eif (ap [evar "isUpper", evar "d"])
                                                                                                                                    (elit (LitChar 'A'))
                                                                                                                                    (elit (LitChar 'a'))], elit (LitInt 10)])])])]]
             (ap [evar "readInt", elit (LitInt 16), evar "isHexDigit", evar "hex"]))])],
    [("readOct",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              tReadS (TGen 0))),
      [([],
        ap [evar "readInt", elit (LitInt 8), evar "isOctDigit", elambda ([PVar "d"],
                                                                         ap [econst minusMfun, ap [econst fromEnumMfun, evar "d"], ap [econst fromEnumMfun, elit (LitChar '0')]])])])],
    [("readDec",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              tReadS (TGen 0))),
      [([],
        ap [evar "readInt", elit (LitInt 10), evar "isDigit", elambda ([PVar "d"],
                                                                       ap [econst minusMfun, ap [econst fromEnumMfun, evar "d"], ap [econst fromEnumMfun, elit (LitChar '0')]])])])],
    [("readLitChar",
      Just (Forall []
             ([] :=>
              tReadS tChar)),
      [([PCon consCfun [PLit (LitChar '\\'), PVar "s"]],
        elet [[("readEsc",
                Nothing,
                [([PCon consCfun [PLit (LitChar 'a'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\a'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 'b'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\b'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 'f'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\f'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 'n'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\n'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 'r'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\r'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 't'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\t'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar 'v'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\v'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar '\\'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\\'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar '"'), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '"'), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar '\''), PVar "s"]],
                  eCons (ap [econst tup2Cfun, elit (LitChar '\''), evar "s"])
                  eNil),
                 ([PCon consCfun [PLit (LitChar '^'), PCon consCfun [PVar "c", PVar "s"]]],
                  eguarded [(ap [evar "&&", ap [econst geMfun, evar "c", elit (LitChar '@')], ap [econst leMfun, evar "c", elit (LitChar '_')]],
                             eCons (ap [econst tup2Cfun, ap [econst toEnumMfun, ap [econst minusMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, elit (LitChar '@')]]], evar "s"])
                             eNil)]),
                 ([PAs "s" (PCon consCfun [PVar "d", PWildcard])],
                  eguarded [(ap [evar "isDigit", evar "d"],
                             eCompFrom (PCon tup2Cfun [PVar "n", PVar "t"]) (ap [evar "readDec", evar "s"])
                             (eListRet (ap [econst tup2Cfun, ap [econst toEnumMfun, evar "n"], evar "t"])))]),
                 ([PCon consCfun [PLit (LitChar 'o'), PVar "s"]],
                  eCompFrom (PCon tup2Cfun [PVar "n", PVar "t"]) (ap [evar "readOct", evar "s"])
                  (eListRet (ap [econst tup2Cfun, ap [econst toEnumMfun, evar "n"], evar "t"]))),
                 ([PCon consCfun [PLit (LitChar 'x'), PVar "s"]],
                  eCompFrom (PCon tup2Cfun [PVar "n", PVar "t"]) (ap [evar "readHex", evar "s"])
                  (eListRet (ap [econst tup2Cfun, ap [econst toEnumMfun, evar "n"], evar "t"]))),
                 ([PAs "s" (PCon consCfun [PVar "c", PWildcard])],
                  eguarded [(ap [evar "isUpper", evar "c"],
                             elet [[("table",
                                     Nothing,
                                     [([],
                                       ap [econst consCfun, ap [econst tup2Cfun, elit (LitChar '\DEL'), elit (LitStr "DEL")], evar "asciiTab"])])]]
                                  (ecase (eCompFrom (PCon tup2Cfun [PVar "c", PVar "mne"]) (evar "table")
                                          (eCompFrom (PCon tup2Cfun [PCon nilCfun [], PVar "s'"]) (eCons (ap [evar "lexmatch", evar "mne", evar "s"])
                                                                                                  eNil)
                                           (eListRet (ap [econst tup2Cfun, evar "c", evar "s'"]))))
                                         [(PCon consCfun [PVar "pr", PWildcard],
                                           eCons (evar "pr")
                                           eNil),
                                          (PCon nilCfun [],
                                           econst nilCfun)]))]),
                 ([PWildcard],
                  econst nilCfun)])]]
             (ap [evar "readEsc", evar "s"])),
       ([PCon consCfun [PVar "c", PVar "s"]],
        eCons (ap [econst tup2Cfun, evar "c", evar "s"])
        eNil)])],
    [("protectEsc",
      Nothing,
      [([PVar "p", PVar "f"],
        elet [[("cont",
                Nothing,
                [([PAs "s" (PCon consCfun [PVar "c", PWildcard])],
                  eguarded [(ap [evar "p", evar "c"],
                             ap [evar "++", elit (LitStr "\\\\&"), evar "s"])]),
                 ([PVar "s"],
                  evar "s")])]]
             (ap [evar ".", evar "f", evar "cont"]))])],
    [("showLitChar",
      Just (Forall []
             ([] :=>
              (tChar `fn` tShowS))),
      [([PVar "c"],
        eguarded [(ap [econst gtMfun, evar "c", elit (LitChar '\DEL')],
                   ap [evar ".", ap [evar "showChar", elit (LitChar '\\')], ap [evar "protectEsc", evar "isDigit", ap [evar "shows", ap [econst fromEnumMfun, evar "c"]]]])]),
       ([PLit (LitChar '\DEL')],
        ap [evar "showString", elit (LitStr "\\\\DEL")]),
       ([PLit (LitChar '\\')],
        ap [evar "showString", elit (LitStr "\\\\\\\\")]),
       ([PVar "c"],
        eguarded [(ap [econst geMfun, evar "c", elit (LitChar ' ')],
                   ap [evar "showChar", evar "c"])]),
       ([PLit (LitChar '\a')],
        ap [evar "showString", elit (LitStr "\\\\a")]),
       ([PLit (LitChar '\b')],
        ap [evar "showString", elit (LitStr "\\\\b")]),
       ([PLit (LitChar '\f')],
        ap [evar "showString", elit (LitStr "\\\\f")]),
       ([PLit (LitChar '\n')],
        ap [evar "showString", elit (LitStr "\\\\n")]),
       ([PLit (LitChar '\r')],
        ap [evar "showString", elit (LitStr "\\\\r")]),
       ([PLit (LitChar '\t')],
        ap [evar "showString", elit (LitStr "\\\\t")]),
       ([PLit (LitChar '\v')],
        ap [evar "showString", elit (LitStr "\\\\v")]),
       ([PLit (LitChar '\SO')],
        ap [evar "protectEsc", ap [econst eqMfun, elit (LitChar 'H')], ap [evar "showString", elit (LitStr "\\\\SO")]]),
       ([PVar "c"],
        ap [evar "showString", ap [econst consCfun, elit (LitChar '\\'), ap [evar "snd", ap [evar "!!", evar "asciiTab", ap [econst fromEnumMfun, evar "c"]]]]])])],
    [("showInt",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (TGen 0 `fn` tShowS))),
      [([PVar "n", PVar "r"],
        eguarded [(ap [econst ltMfun, evar "n", elit (LitInt 0)],
                   ap [evar "error", elit (LitStr "Numeric.showInt: can't show negative numbers")]),
                  (evar "otherwise",
                   elet [[("v253",
                           Nothing,
                           [([], ap [econst quotRemMfun, evar "n", elit (LitInt 10)])]),
                          ("n'",
                           Nothing,
                           [([], ecase (evar "v253") [(PCon tup2Cfun [PVar "n'", PVar "d"], evar "n'")])]),
                          ("d",
                           Nothing,
                           [([], ecase (evar "v253") [(PCon tup2Cfun [PVar "n'", PVar "d"], evar "d")])])],
                         [("r'",
                           Nothing,
                           [([],
                             ap [econst consCfun, ap [econst toEnumMfun, ap [econst plusMfun, ap [econst fromEnumMfun, elit (LitChar '0')], ap [evar "fromIntegral", evar "d"]]], evar "r"])])]]
                        (eif (ap [econst eqMfun, evar "n'", elit (LitInt 0)])
                             (evar "r'")
                             (ap [evar "showInt", evar "n'", evar "r'"])))])])],
    [("readSigned",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              (tReadS (TGen 0) `fn` tReadS (TGen 0)))),
      [([PVar "readPos"],
        elet [[("read''",
                Nothing,
                [([PVar "r"],
                  eCompFrom (PCon tup2Cfun [PVar "str", PVar "s"]) (ap [evar "lex", evar "r"])
                  (eCompFrom (PCon tup2Cfun [PVar "n", PLit (LitStr "")]) (ap [evar "readPos", evar "str"])
                   (eListRet (ap [econst tup2Cfun, evar "n", evar "s"]))))])],
              [("read'",
                Nothing,
                [([PVar "r"],
                  ap [evar "++", ap [evar "read''", evar "r"], eCompFrom (PCon tup2Cfun [PLit (LitStr "-"), PVar "s"]) (ap [evar "lex", evar "r"])
                                                               (eCompFrom (PCon tup2Cfun [PVar "x", PVar "t"]) (ap [evar "read''", evar "s"])
                                                                (eListRet (ap [econst tup2Cfun, ap [econst negateMfun, evar "x"], evar "t"])))])])]]
             (ap [evar "readParen", econst falseCfun, evar "read'"]))])],
    [("showSigned",
      Just (Forall [Star]
             ([isIn1 cReal (TGen 0)] :=>
              ((TGen 0 `fn` tShowS) `fn` tInt `fn` TGen 0 `fn` tShowS))),
      [([PVar "showPos", PVar "p", PVar "x"],
        eif (ap [econst ltMfun, evar "x", elit (LitInt 0)])
            (ap [evar "showParen", ap [econst gtMfun, evar "p", elit (LitInt 6)], ap [evar ".", ap [evar "showChar", elit (LitChar '-')], ap [evar "showPos", ap [econst negateMfun, evar "x"]]]])
            (ap [evar "showPos", evar "x"]))])],
    [("readFloat",
      Just (Forall [Star]
             ([isIn1 cRealFloat (TGen 0)] :=>
              tReadS (TGen 0))),
      [([PVar "r"],
        elet [[("lexFrac",
                Nothing,
                [([PCon consCfun [PLit (LitChar '.'), PVar "s"]],
                  ap [evar "lexDigits", evar "s"]),
                 ([PVar "s"],
                  eCons (ap [econst tup2Cfun, elit (LitStr ""), evar "s"])
                  eNil)])],
              [("readFix",
                Nothing,
                [([PVar "r"],
                  eCompFrom (PCon tup2Cfun [PVar "ds", PVar "s"]) (ap [evar "lexDigits", evar "r"])
                  (eCompFrom (PCon tup2Cfun [PVar "ds'", PVar "t"]) (ap [evar "lexFrac", evar "s"])
                   (eListRet (ap [econst tup3Cfun, ap [evar "read", ap [evar "++", evar "ds", evar "ds'"]], ap [evar "length", evar "ds'"], evar "t"]))))])],
              [("readExp'",
                Nothing,
                [([PCon consCfun [PLit (LitChar '-'), PVar "s"]],
                  eCompFrom (PCon tup2Cfun [PVar "k", PVar "t"]) (ap [evar "readDec", evar "s"])
                  (eListRet (ap [econst tup2Cfun, ap [econst negateMfun, evar "k"], evar "t"]))),
                 ([PCon consCfun [PLit (LitChar '+'), PVar "s"]],
                  ap [evar "readDec", evar "s"]),
                 ([PVar "s"],
                  ap [evar "readDec", evar "s"])])],
              [("readExp",
                Nothing,
                [([PCon consCfun [PVar "e", PVar "s"]],
                  eguarded [(ap [evar "elem", evar "e", elit (LitStr "eE")],
                             ap [evar "readExp'", evar "s"])]),
                 ([PVar "s"],
                  eCons (ap [econst tup2Cfun, elit (LitInt 0), evar "s"])
                  eNil)])]]
             (eCompFrom (PCon tup3Cfun [PVar "n", PVar "d", PVar "s"]) (ap [evar "readFix", evar "r"])
              (eCompFrom (PCon tup2Cfun [PVar "k", PVar "t"]) (ap [evar "readExp", evar "s"])
               (eListRet (ap [econst tup2Cfun, ap [econst fromRationalMfun, ap [econst timesMfun, ap [evar "%", evar "n", elit (LitInt 1)], ap [evar "^^", elit (LitInt 10), ap [econst minusMfun, evar "k", evar "d"]]]], evar "t"])))))])],
    [("putStrLn",
      Just (Forall []
             ([] :=>
              (tString `fn` TAp tIO tUnit))),
      [([PVar "s"],
        eCompFrom PWildcard (ap [evar "putStr", evar "s"])
        (ap [evar "putChar", elit (LitChar '\n')]))])],
    [("print",
      Just (Forall [Star]
             ([isIn1 cShow (TGen 0)] :=>
              (TGen 0 `fn` TAp tIO tUnit))),
      [([],
        ap [evar ".", evar "putStrLn", econst showMfun])])],
    [("getLine",
      Just (Forall []
             ([] :=>
              (TAp tIO tString))),
      [([],
        eCompFrom (PVar "c") (evar "getChar")
        (eif (ap [econst eqMfun, evar "c", elit (LitChar '\n')])
             (ap [econst returnMfun, elit (LitStr "")])
             (eCompFrom (PVar "cs") (evar "getLine")
              (ap [econst returnMfun, ap [econst consCfun, evar "c", evar "cs"]]))))])],
    [("readIO",
      Just (Forall [Star]
             ([isIn1 cRead (TGen 0)] :=>
              (tString `fn` TAp tIO (TGen 0)))),
      [([PVar "s"],
        ecase (eCompFrom (PCon tup2Cfun [PVar "x", PVar "t"]) (ap [evar "reads", evar "s"])
               (eCompFrom (PCon tup2Cfun [PLit (LitStr ""), PLit (LitStr "")]) (ap [evar "lex", evar "t"])
                (eListRet (evar "x"))))
              [(pCons (PVar "x") pNil,
                ap [econst returnMfun, evar "x"]),
               (PCon nilCfun [],
                ap [evar "ioError", ap [evar "userError", elit (LitStr "PreludeIO.readIO: no parse")]]),
               (PWildcard,
                ap [evar "ioError", ap [evar "userError", elit (LitStr "PreludeIO.readIO: ambiguous parse")]])])])],
    [("readLn",
      Just (Forall [Star]
             ([isIn1 cRead (TGen 0)] :=>
              (TAp tIO (TGen 0)))),
      [([],
        eCompFrom (PVar "l") (evar "getLine")
        (eCompFrom (PVar "r") (ap [evar "readIO", evar "l"])
         (ap [econst returnMfun, evar "r"])))])],
    [("interact",
      Just (Forall []
             ([] :=>
              ((tString `fn` tString) `fn` TAp tIO tUnit))),
      [([PVar "f"],
        ap [econst mbindMfun, evar "getContents", ap [evar ".", evar "putStr", evar "f"]])])]]

hugsSpecific :: [BindGroup]
hugsSpecific
 = map toBg
   [[("hugsPutStr",
      Just (Forall []
             ([] :=>
              (tString `fn` TAp tIO tUnit))),
      [([],
        evar "putStr")])],
    [("primExitWith",
      Just (Forall [Star]
             ([] :=>
              (tInt `fn` TAp tIO (TGen 0)))),
      [([PVar "c"],
        ap [econst iOCfun, elambda ([PVar "f", PVar "s"],
                                    ap [econst hugs_ExitWithCfun, evar "c"])])])],
    [("hugsIORun",
      Just (Forall [Star]
             ([] :=>
              (TAp tIO (TGen 0) `fn` TAp (TAp tEither tInt) (TGen 0)))),
      [([PVar "m"],
        elet [[("performIO",
                Just (Forall [Star]
                       ([] :=>
                        (TAp tIO (TGen 0) `fn` TAp (TAp tEither tInt) (TGen 0)))),
                [([PCon iOCfun [PVar "m"]],
                  ecase (ap [evar "m", econst hugs_ErrorCfun, econst hugs_ReturnCfun])
                        [(PCon hugs_ReturnCfun [PVar "a"],
                          ap [econst rightCfun, evar "a"]),
                         (PCon hugs_ExitWithCfun [PVar "e"],
                          ap [econst leftCfun, evar "e"]),
                         (PWildcard,
                          ap [econst leftCfun, elit (LitInt 1)])])])],
              [("runAndShowError",
                Just (Forall [Star]
                       ([] :=>
                        (TAp tIO (TGen 0) `fn` TAp tIO (TGen 0)))),
                [([PVar "m"],
                  ap [evar "catch", evar "m", elambda ([PVar "err"],
                                                       eCompFrom PWildcard (ap [evar "putChar", elit (LitChar '\n')])
                                                       (eCompFrom PWildcard (ap [evar "putStr", ap [evar "ioeGetErrorString", evar "err"]])
                                                        (ap [evar "primExitWith", elit (LitInt 1)])))])])]]
             (ap [evar "performIO", ap [evar "runAndShowError", evar "m"]]))])],
    [("primCompAux",
      Just (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` tOrdering `fn` tOrdering))),
      [([PVar "x", PVar "y", PVar "o"],
        ecase (ap [econst compareMfun, evar "x", evar "y"])
              [(PCon eQCfun [],
                evar "o"),
               (PCon lTCfun [],
                econst lTCfun),
               (PCon gTCfun [],
                econst gTCfun)])])],
    [("primPmInt",
      Just (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (tInt `fn` TGen 0 `fn` tBool))),
      [([PVar "n", PVar "x"],
        ap [econst eqMfun, ap [econst fromIntMfun, evar "n"], evar "x"])])],
    [("primPmInteger",
      Just (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (tInteger `fn` TGen 0 `fn` tBool))),
      [([PVar "n", PVar "x"],
        ap [econst eqMfun, ap [econst fromIntegerMfun, evar "n"], evar "x"])])],
    [("primPmFlt",
      Just (Forall [Star]
             ([isIn1 cFractional (TGen 0)] :=>
              (tDouble `fn` TGen 0 `fn` tBool))),
      [([PVar "n", PVar "x"],
        ap [econst eqMfun, ap [econst fromDoubleMfun, evar "n"], evar "x"])])],
    [("primPmNpk",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (tInt `fn` TGen 0 `fn` TAp tMaybe (TGen 0)))),
      [([PVar "n", PVar "x"],
        elet [[("n'",
                Nothing,
                [([],
                  ap [econst fromIntMfun, evar "n"])])]]
             (eif (ap [econst leMfun, evar "n'", evar "x"])
                  (ap [econst justCfun, ap [econst minusMfun, evar "x", evar "n'"]])
                  (econst nothingCfun)))])],
    [("primPmSub",
      Just (Forall [Star]
             ([isIn1 cIntegral (TGen 0)] :=>
              (tInt `fn` TGen 0 `fn` TGen 0))),
      [([PVar "n", PVar "x"],
        ap [econst minusMfun, evar "x", ap [econst fromIntMfun, evar "n"]])])]]

-----------------------------------------------------------------------------
-- Implementations of members:

preludeMems :: [BindGroup]
preludeMems
 = map (\x -> toBg [x])
   [("v256",
     Just (Forall [Kfun Star Star, Star, Star]
            ([isIn1 cMonad (TGen 0)] :=>
             (TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 2)))),
     [([PVar "p", PVar "q"],
       ap [econst mbindMfun, evar "p", elambda ([PWildcard],
                                                evar "q")])]),
    ("v257",
     Just (Forall [Kfun Star Star, Star]
            ([isIn1 cMonad (TGen 0)] :=>
             (tString `fn` TAp (TGen 0) (TGen 1)))),
     [([PVar "s"],
       ap [evar "error", evar "s"])]),
    ("v259",
     Just (Forall [Star]
            ([isIn1 cShow (TGen 0)] :=>
             (TGen 0 `fn` tString))),
     [([PVar "x"],
       ap [econst showsPrecMfun, elit (LitInt 0), evar "x", elit (LitStr "")])]),
    ("v260",
     Just (Forall [Star]
            ([isIn1 cShow (TGen 0)] :=>
             (tInt `fn` TGen 0 `fn` tShowS))),
     [([PWildcard, PVar "x", PVar "s"],
       ap [evar "++", ap [econst showMfun, evar "x"], evar "s"])]),
    ("v261",
     Just (Forall [Star]
            ([isIn1 cShow (TGen 0)] :=>
             (TAp tList (TGen 0) `fn` tShowS))),
     [([PCon nilCfun []],
       ap [evar "showString", elit (LitStr "[]")]),
      ([PCon consCfun [PVar "x", PVar "xs"]],
       elet [[("showl",
               Nothing,
               [([PCon nilCfun []],
                 ap [evar "showChar", elit (LitChar ']')]),
                ([PCon consCfun [PVar "x", PVar "xs"]],
                 ap [evar ".", ap [evar "showChar", elit (LitChar ',')], ap [evar ".", ap [evar "shows", evar "x"], ap [evar "showl", evar "xs"]]])])]]
            (ap [evar ".", ap [evar "showChar", elit (LitChar '[')], ap [evar ".", ap [evar "shows", evar "x"], ap [evar "showl", evar "xs"]]]))]),
    ("v263",
     Just (Forall [Star]
            ([isIn1 cRead (TGen 0)] :=>
             tReadS (TAp tList (TGen 0)))),
     [([],
       elet [[("readl'",
               Nothing,
               [([PVar "s"],
                 ap [evar "++", eCompFrom (PCon tup2Cfun [PLit (LitStr "]"), PVar "t"]) (ap [evar "lex", evar "s"])
                                (eListRet (ap [econst tup2Cfun, econst nilCfun, evar "t"])), eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "t"]) (ap [evar "lex", evar "s"])
                                                                                             (eCompFrom (PCon tup2Cfun [PVar "x", PVar "u"]) (ap [evar "reads", evar "t"])
                                                                                              (eCompFrom (PCon tup2Cfun [PVar "xs", PVar "v"]) (ap [evar "readl'", evar "u"])
                                                                                               (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "xs"], evar "v"]))))])])],
             [("readl",
               Nothing,
               [([PVar "s"],
                 ap [evar "++", eCompFrom (PCon tup2Cfun [PLit (LitStr "]"), PVar "t"]) (ap [evar "lex", evar "s"])
                                (eListRet (ap [econst tup2Cfun, econst nilCfun, evar "t"])), eCompFrom (PCon tup2Cfun [PVar "x", PVar "t"]) (ap [evar "reads", evar "s"])
                                                                                             (eCompFrom (PCon tup2Cfun [PVar "xs", PVar "u"]) (ap [evar "readl'", evar "t"])
                                                                                              (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "x", evar "xs"], evar "u"])))])])]]
            (ap [evar "readParen", econst falseCfun, elambda ([PVar "r"],
                                                              eCompFrom (PCon tup2Cfun [PLit (LitStr "["), PVar "s"]) (ap [evar "lex", evar "r"])
                                                              (eCompFrom (PVar "pr") (ap [evar "readl", evar "s"])
                                                               (eListRet (evar "pr"))))]))]),
    ("v264",
     Just (Forall [Star]
            ([isIn1 cEnum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([],
       ap [evar ".", econst toEnumMfun, ap [evar ".", ap [econst plusMfun, elit (LitInt 1)], econst fromEnumMfun]])]),
    ("v265",
     Just (Forall [Star]
            ([isIn1 cEnum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([],
       ap [evar ".", econst toEnumMfun, ap [evar ".", ap [evar "subtract", elit (LitInt 1)], econst fromEnumMfun]])]),
    ("v270",
     Just (Forall [Star]
            ([isIn1 cEnum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     [([PVar "x", PVar "y"],
       ap [evar "map", econst toEnumMfun, ap [econst enumFromToMfun, ap [econst fromEnumMfun, evar "x"], ap [econst fromEnumMfun, evar "y"]]])]),
    ("v271",
     Just (Forall [Star]
            ([isIn1 cEnum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     [([PVar "x", PVar "y", PVar "z"],
       ap [evar "map", econst toEnumMfun, ap [econst enumFromThenToMfun, ap [econst fromEnumMfun, evar "x"], ap [econst fromEnumMfun, evar "y"], ap [econst fromEnumMfun, evar "z"]]])]),
    ("v276",
     Just (Forall [Star]
            ([isIn1 cIx (TGen 0)] :=>
             (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` tInt))),
     [([PAs "r" (PCon tup2Cfun [PVar "l", PVar "u"])],
       eguarded [(ap [econst gtMfun, evar "l", evar "u"],
                  elit (LitInt 0)),
                 (evar "otherwise",
                  ap [econst plusMfun, ap [econst indexMfun, evar "r", evar "u"], elit (LitInt 1)])])]),
    ("v284",
     Just (Forall [Star]
            ([isIn1 cRealFloat (TGen 0)] :=>
             (TGen 0 `fn` tInt))),
     [([PVar "x"],
       elet [[("v285",
               Nothing,
               [([], ap [econst decodeFloatMfun, evar "x"])]),
              ("m",
               Nothing,
               [([], ecase (evar "v285") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "m")])]),
              ("n",
               Nothing,
               [([], ecase (evar "v285") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "n")])])]]
            (eif (ap [econst eqMfun, evar "m", elit (LitInt 0)])
                 (elit (LitInt 0))
                 (ap [econst plusMfun, evar "n", ap [econst floatDigitsMfun, evar "x"]])))]),
    ("v286",
     Just (Forall [Star]
            ([isIn1 cRealFloat (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       elet [[("v287",
               Nothing,
               [([], ap [econst decodeFloatMfun, evar "x"])]),
              ("m",
               Nothing,
               [([], ecase (evar "v287") [(PCon tup2Cfun [PVar "m", PWildcard], evar "m")])])]]
            (ap [econst encodeFloatMfun, evar "m", ap [econst negateMfun, ap [econst floatDigitsMfun, evar "x"]]]))]),
    ("v288",
     Just (Forall [Star]
            ([isIn1 cRealFloat (TGen 0)] :=>
             (tInt `fn` TGen 0 `fn` TGen 0))),
     [([PVar "k", PVar "x"],
       elet [[("v289",
               Nothing,
               [([], ap [econst decodeFloatMfun, evar "x"])]),
              ("m",
               Nothing,
               [([], ecase (evar "v289") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "m")])]),
              ("n",
               Nothing,
               [([], ecase (evar "v289") [(PCon tup2Cfun [PVar "m", PVar "n"], evar "n")])])]]
            (ap [econst encodeFloatMfun, evar "m", ap [econst plusMfun, evar "n", evar "k"]]))]),
    ("v295",
     Just (Forall [Star]
            ([isIn1 cRealFloat (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "y", PVar "x"],
       eguarded [(ap [econst gtMfun, evar "x", elit (LitInt 0)],
                  ap [econst atanMfun, ap [econst divideMfun, evar "y", evar "x"]]),
                 (ap [evar "&&", ap [econst eqMfun, evar "x", elit (LitInt 0)], ap [econst gtMfun, evar "y", elit (LitInt 0)]],
                  ap [econst divideMfun, econst piMfun, elit (LitInt 2)]),
                 (ap [evar "&&", ap [econst ltMfun, evar "x", elit (LitInt 0)], ap [econst gtMfun, evar "y", elit (LitInt 0)]],
                  ap [econst plusMfun, econst piMfun, ap [econst atanMfun, ap [econst divideMfun, evar "y", evar "x"]]]),
                 (ap [evar "||", ap [evar "&&", ap [econst leMfun, evar "x", elit (LitInt 0)], ap [econst ltMfun, evar "y", elit (LitInt 0)]], ap [evar "||", ap [evar "&&", ap [econst ltMfun, evar "x", elit (LitInt 0)], ap [econst isNegativeZeroMfun, evar "y"]], ap [evar "&&", ap [econst isNegativeZeroMfun, evar "x"], ap [econst isNegativeZeroMfun, evar "y"]]]],
                  ap [econst negateMfun, ap [econst atan2Mfun, ap [econst negateMfun, evar "y"], evar "x"]]),
                 (ap [evar "&&", ap [econst eqMfun, evar "y", elit (LitInt 0)], ap [evar "||", ap [econst ltMfun, evar "x", elit (LitInt 0)], ap [econst isNegativeZeroMfun, evar "x"]]],
                  econst piMfun),
                 (ap [evar "&&", ap [econst eqMfun, evar "x", elit (LitInt 0)], ap [econst eqMfun, evar "y", elit (LitInt 0)]],
                  evar "y"),
                 (evar "otherwise",
                  ap [econst plusMfun, evar "x", evar "y"])])]),
    ("v299",
     Just (Forall [Star, Star]
            ([isIn1 cRealFrac (TGen 0),
              isIn1 cIntegral (TGen 1)] :=>
             (TGen 0 `fn` TGen 1))),
     [([PVar "x"],
       elet [[("v300",
               Nothing,
               [([], ap [econst properFractionMfun, evar "x"])]),
              ("m",
               Nothing,
               [([], ecase (evar "v300") [(PCon tup2Cfun [PVar "m", PWildcard], evar "m")])])]]
            (evar "m"))]),
    ("v301",
     Just (Forall [Star, Star]
            ([isIn1 cRealFrac (TGen 0),
              isIn1 cIntegral (TGen 1)] :=>
             (TGen 0 `fn` TGen 1))),
     [([PVar "x"],
       elet [[("v302",
               Nothing,
               [([], ap [econst properFractionMfun, evar "x"])]),
              ("n",
               Nothing,
               [([], ecase (evar "v302") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "n")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v302") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "r")])])],
             [("m",
               Nothing,
               [([],
                 eif (ap [econst ltMfun, evar "r", elit (LitInt 0)])
                     (ap [econst minusMfun, evar "n", elit (LitInt 1)])
                     (ap [econst plusMfun, evar "n", elit (LitInt 1)]))])]]
            (ecase (ap [econst signumMfun, ap [econst minusMfun, ap [econst absMfun, evar "r"], elit (LitRat 0.5)]])
                   [(PLit (LitInt (-1)),
                     evar "n"),
                    (PLit (LitInt 0),
                     eif (ap [econst evenMfun, evar "n"])
                         (evar "n")
                         (evar "m")),
                    (PLit (LitInt 1),
                     evar "m")]))]),
    ("v303",
     Just (Forall [Star, Star]
            ([isIn1 cRealFrac (TGen 0),
              isIn1 cIntegral (TGen 1)] :=>
             (TGen 0 `fn` TGen 1))),
     [([PVar "x"],
       elet [[("v304",
               Nothing,
               [([], ap [econst properFractionMfun, evar "x"])]),
              ("n",
               Nothing,
               [([], ecase (evar "v304") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "n")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v304") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "r")])])]]
            (eif (ap [econst gtMfun, evar "r", elit (LitInt 0)])
                 (ap [econst plusMfun, evar "n", elit (LitInt 1)])
                 (evar "n")))]),
    ("v305",
     Just (Forall [Star, Star]
            ([isIn1 cRealFrac (TGen 0),
              isIn1 cIntegral (TGen 1)] :=>
             (TGen 0 `fn` TGen 1))),
     [([PVar "x"],
       elet [[("v306",
               Nothing,
               [([], ap [econst properFractionMfun, evar "x"])]),
              ("n",
               Nothing,
               [([], ecase (evar "v306") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "n")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v306") [(PCon tup2Cfun [PVar "n", PVar "r"], evar "r")])])]]
            (eif (ap [econst ltMfun, evar "r", elit (LitInt 0)])
                 (ap [econst minusMfun, evar "n", elit (LitInt 1)])
                 (evar "n")))]),
    ("v308",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0))),
     [([],
       ap [econst timesMfun, elit (LitInt 4), ap [econst atanMfun, elit (LitInt 1)]])]),
    ("v311",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst starstarMfun, evar "x", elit (LitRat 0.5)])]),
    ("v312",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       ap [econst expMfun, ap [econst timesMfun, ap [econst logMfun, evar "x"], evar "y"]])]),
    ("v313",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       ap [econst divideMfun, ap [econst logMfun, evar "y"], ap [econst logMfun, evar "x"]])]),
    ("v316",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, ap [econst sinMfun, evar "x"], ap [econst cosMfun, evar "x"]])]),
    ("v320",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, ap [econst minusMfun, ap [econst expMfun, evar "x"], ap [econst expMfun, ap [econst negateMfun, evar "x"]]], elit (LitInt 2)])]),
    ("v321",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, ap [econst plusMfun, ap [econst expMfun, evar "x"], ap [econst expMfun, ap [econst negateMfun, evar "x"]]], elit (LitInt 2)])]),
    ("v322",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, ap [econst sinhMfun, evar "x"], ap [econst coshMfun, evar "x"]])]),
    ("v323",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst logMfun, ap [econst plusMfun, evar "x", ap [econst sqrtMfun, ap [econst plusMfun, ap [econst timesMfun, evar "x", evar "x"], elit (LitInt 1)]]]])]),
    ("v324",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst logMfun, ap [econst plusMfun, evar "x", ap [econst sqrtMfun, ap [econst minusMfun, ap [econst timesMfun, evar "x", evar "x"], elit (LitInt 1)]]]])]),
    ("v325",
     Just (Forall [Star]
            ([isIn1 cFloating (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, ap [econst minusMfun, ap [econst logMfun, ap [econst plusMfun, elit (LitInt 1), evar "x"]], ap [econst logMfun, ap [econst minusMfun, elit (LitInt 1), evar "x"]]], elit (LitInt 2)])]),
    ("v327",
     Just (Forall [Star]
            ([isIn1 cFractional (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       ap [econst timesMfun, evar "x", ap [econst recipMfun, evar "y"]])]),
    ("v328",
     Just (Forall [Star]
            ([isIn1 cFractional (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst divideMfun, elit (LitInt 1), evar "x"])]),
    ("v330",
     Just (Forall [Star]
            ([isIn1 cFractional (TGen 0)] :=>
             (tDouble `fn` TGen 0))),
     [([],
       ap [evar ".", econst fromRationalMfun, econst toRationalMfun])]),
    ("v333",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "n", PVar "d"],
       elet [[("v334",
               Nothing,
               [([], ap [econst quotRemMfun, evar "n", evar "d"])]),
              ("q",
               Nothing,
               [([], ecase (evar "v334") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v334") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])]]
            (evar "q"))]),
    ("v335",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "n", PVar "d"],
       elet [[("v336",
               Nothing,
               [([], ap [econst quotRemMfun, evar "n", evar "d"])]),
              ("q",
               Nothing,
               [([], ecase (evar "v336") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v336") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])]]
            (evar "r"))]),
    ("v337",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "n", PVar "d"],
       elet [[("v338",
               Nothing,
               [([], ap [econst divModMfun, evar "n", evar "d"])]),
              ("q",
               Nothing,
               [([], ecase (evar "v338") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v338") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])]]
            (evar "q"))]),
    ("v339",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "n", PVar "d"],
       elet [[("v340",
               Nothing,
               [([], ap [econst divModMfun, evar "n", evar "d"])]),
              ("q",
               Nothing,
               [([], ecase (evar "v340") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v340") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])]]
            (evar "r"))]),
    ("v342",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0)))),
     [([PVar "n", PVar "d"],
       elet [[("v343",
               Nothing,
               [([], ap [econst quotRemMfun, evar "n", evar "d"])]),
              ("qr",
               Nothing,
               [([], ecase (evar "v343") [(PAs "qr" (PCon tup2Cfun [PVar "q", PVar "r"]), evar "qr")])]),
              ("q",
               Nothing,
               [([], ecase (evar "v343") [(PAs "qr" (PCon tup2Cfun [PVar "q", PVar "r"]), evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v343") [(PAs "qr" (PCon tup2Cfun [PVar "q", PVar "r"]), evar "r")])])]]
            (eif (ap [econst eqMfun, ap [econst signumMfun, evar "r"], ap [econst negateMfun, ap [econst signumMfun, evar "d"]]])
                 (ap [econst tup2Cfun, ap [econst minusMfun, evar "q", elit (LitInt 1)], ap [econst plusMfun, evar "r", evar "d"]])
                 (evar "qr")))]),
    ("v344",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` tBool))),
     [([PVar "n"],
       ap [econst eqMfun, ap [econst remMfun, evar "n", elit (LitInt 2)], elit (LitInt 0)])]),
    ("v345",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` tBool))),
     [([],
       ap [evar ".", evar "not", econst evenMfun])]),
    ("v347",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TGen 0 `fn` tInt))),
     [([],
       ap [evar ".", econst toIntMfun, econst toIntegerMfun])]),
    ("v354",
     Just (Forall [Star]
            ([isIn1 cNum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       ap [econst plusMfun, evar "x", ap [econst negateMfun, evar "y"]])]),
    ("v356",
     Just (Forall [Star]
            ([isIn1 cNum (TGen 0)] :=>
             (TGen 0 `fn` TGen 0))),
     [([PVar "x"],
       ap [econst minusMfun, elit (LitInt 0), evar "x"])]),
    ("v360",
     Just (Forall [Star]
            ([isIn1 cNum (TGen 0)] :=>
             (tInt `fn` TGen 0))),
     [([],
       evar "fromIntegral")]),
    ("v364",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tOrdering))),
     [([PVar "x", PVar "y"],
       eguarded [(ap [econst eqMfun, evar "x", evar "y"],
                  econst eQCfun),
                 (ap [econst leMfun, evar "x", evar "y"],
                  econst lTCfun),
                 (evar "otherwise",
                  econst gTCfun)])]),
    ("v365",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [econst eqMfun, ap [econst compareMfun, evar "x", evar "y"], econst lTCfun])]),
    ("v366",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [econst neqMfun, ap [econst compareMfun, evar "x", evar "y"], econst gTCfun])]),
    ("v367",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [econst neqMfun, ap [econst compareMfun, evar "x", evar "y"], econst lTCfun])]),
    ("v368",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [econst eqMfun, ap [econst compareMfun, evar "x", evar "y"], econst gTCfun])]),
    ("v369",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       eguarded [(ap [econst geMfun, evar "x", evar "y"],
                  evar "x"),
                 (evar "otherwise",
                  evar "y")])]),
    ("v370",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` TGen 0))),
     [([PVar "x", PVar "y"],
       eguarded [(ap [econst leMfun, evar "x", evar "y"],
                  evar "x"),
                 (evar "otherwise",
                  evar "y")])]),
    ("v371",
     Just (Forall [Star]
            ([isIn1 cEq (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [evar "not", ap [econst neqMfun, evar "x", evar "y"]])]),
    ("v372",
     Just (Forall [Star]
            ([isIn1 cEq (TGen 0)] :=>
             (TGen 0 `fn` TGen 0 `fn` tBool))),
     [([PVar "x", PVar "y"],
       ap [evar "not", ap [econst eqMfun, evar "x", evar "y"]])]),
    ("v374",
     Just (Forall []
            ([isIn1 cEq tUnit] :=>
             (tUnit `fn` tUnit `fn` tBool))),
     [([PCon unitCfun [], PCon unitCfun []],
       econst trueCfun)]),
    ("v378",
     Just (Forall []
            ([isIn1 cOrd tUnit] :=>
             (tUnit `fn` tUnit `fn` tOrdering))),
     [([PCon unitCfun [], PCon unitCfun []],
       econst eQCfun)]),
    ("v387",
     Just (Forall []
            ([isIn1 cIx tUnit] :=>
             (TAp (TAp tTuple2 tUnit) tUnit `fn` TAp tList tUnit))),
     [([PCon tup2Cfun [PCon unitCfun [], PCon unitCfun []]],
       eCons (econst unitCfun)
       eNil)]),
    ("v388",
     Just (Forall []
            ([isIn1 cIx tUnit] :=>
             (TAp (TAp tTuple2 tUnit) tUnit `fn` tUnit `fn` tInt))),
     [([PCon tup2Cfun [PCon unitCfun [], PCon unitCfun []], PCon unitCfun []],
       elit (LitInt 0))]),
    ("v389",
     Just (Forall []
            ([isIn1 cIx tUnit] :=>
             (TAp (TAp tTuple2 tUnit) tUnit `fn` tUnit `fn` tBool))),
     [([PCon tup2Cfun [PCon unitCfun [], PCon unitCfun []], PCon unitCfun []],
       econst trueCfun)]),
    ("v394",
     Just (Forall []
            ([isIn1 cEnum tUnit] :=>
             (tInt `fn` tUnit))),
     [([PLit (LitInt 0)],
       econst unitCfun)]),
    ("v395",
     Just (Forall []
            ([isIn1 cEnum tUnit] :=>
             (tUnit `fn` tInt))),
     [([PCon unitCfun []],
       elit (LitInt 0))]),
    ("v396",
     Just (Forall []
            ([isIn1 cEnum tUnit] :=>
             (tUnit `fn` TAp tList tUnit))),
     [([PCon unitCfun []],
       eCons (econst unitCfun)
       eNil)]),
    ("v397",
     Just (Forall []
            ([isIn1 cEnum tUnit] :=>
             (tUnit `fn` tUnit `fn` TAp tList tUnit))),
     [([PCon unitCfun [], PCon unitCfun []],
       eCons (econst unitCfun)
       eNil)]),
    ("v401",
     Just (Forall []
            ([isIn1 cRead tUnit] :=>
             (tInt `fn` tReadS tUnit))),
     [([PVar "p"],
       ap [evar "readParen", econst falseCfun, elambda ([PVar "r"],
                                                        eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "s"]) (ap [evar "lex", evar "r"])
                                                        (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "t"]) (ap [evar "lex", evar "s"])
                                                         (eListRet (ap [econst tup2Cfun, econst unitCfun, evar "t"]))))])]),
    ("v405",
     Just (Forall []
            ([isIn1 cShow tUnit] :=>
             (tInt `fn` tUnit `fn` tShowS))),
     [([PVar "p", PCon unitCfun []],
       ap [evar "showString", elit (LitStr "()")])]),
    ("v408",
     Just (Forall []
            ([isIn1 cBounded tUnit] :=>
             tUnit)),
     [([],
       econst unitCfun)]),
    ("v409",
     Just (Forall []
            ([isIn1 cBounded tUnit] :=>
             tUnit)),
     [([],
       econst unitCfun)]),
    ("v411",
     Just (Forall []
            ([isIn1 cEq tChar] :=>
             (tChar `fn` tChar `fn` tBool))),
     [([],
       evar "primEqChar")]),
    ("v415",
     Just (Forall []
            ([isIn1 cOrd tChar] :=>
             (tChar `fn` tChar `fn` tOrdering))),
     [([],
       evar "primCmpChar")]),
    ("v425",
     Just (Forall []
            ([isIn1 cEnum tChar] :=>
             (tInt `fn` tChar))),
     [([],
       evar "primIntToChar")]),
    ("v426",
     Just (Forall []
            ([isIn1 cEnum tChar] :=>
             (tChar `fn` tInt))),
     [([],
       evar "primCharToInt")]),
    ("v427",
     Just (Forall []
            ([isIn1 cEnum tChar] :=>
             (tChar `fn` TAp tList tChar))),
     [([PVar "c"],
       ap [evar "map", econst toEnumMfun, ap [econst enumFromToMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, esign (econst maxBoundMfun) (Forall []
                                                                                                                                                                ([] :=>
                                                                                                                                                                 tChar))]]])]),
    ("v428",
     Just (Forall []
            ([isIn1 cEnum tChar] :=>
             (tChar `fn` tChar `fn` TAp tList tChar))),
     [([PVar "c", PVar "d"],
       elet [[("lastChar",
               Nothing,
               [([],
                 eif (ap [econst ltMfun, evar "d", evar "c"])
                     (econst minBoundMfun)
                     (econst maxBoundMfun))])]]
            (ap [evar "map", econst toEnumMfun, ap [econst enumFromThenToMfun, ap [econst fromEnumMfun, evar "c"], ap [econst fromEnumMfun, evar "d"], ap [econst fromEnumMfun, esign (evar "lastChar") (Forall []
                                                                                                                                                                                                          ([] :=>
                                                                                                                                                                                                           tChar))]]]))]),
    ("v433",
     Just (Forall []
            ([isIn1 cIx tChar] :=>
             (TAp (TAp tTuple2 tChar) tChar `fn` TAp tList tChar))),
     [([PCon tup2Cfun [PVar "c", PVar "c'"]],
       ap [econst enumFromToMfun, evar "c", evar "c'"])]),
    ("v434",
     Just (Forall []
            ([isIn1 cIx tChar] :=>
             (TAp (TAp tTuple2 tChar) tChar `fn` tChar `fn` tInt))),
     [([PAs "b" (PCon tup2Cfun [PVar "c", PVar "c'"]), PVar "ci"],
       eguarded [(ap [econst inRangeMfun, evar "b", evar "ci"],
                  ap [econst minusMfun, ap [econst fromEnumMfun, evar "ci"], ap [econst fromEnumMfun, evar "c"]]),
                 (evar "otherwise",
                  ap [evar "error", elit (LitStr "Ix.index: Index out of range.")])])]),
    ("v435",
     Just (Forall []
            ([isIn1 cIx tChar] :=>
             (TAp (TAp tTuple2 tChar) tChar `fn` tChar `fn` tBool))),
     [([PCon tup2Cfun [PVar "c", PVar "c'"], PVar "ci"],
       elet [[("i",
               Nothing,
               [([],
                 ap [econst fromEnumMfun, evar "ci"])])]]
            (ap [evar "&&", ap [econst leMfun, ap [econst fromEnumMfun, evar "c"], evar "i"], ap [econst leMfun, evar "i", ap [econst fromEnumMfun, evar "c'"]]]))]),
    ("v438",
     Just (Forall []
            ([isIn1 cRead tChar] :=>
             (tInt `fn` tReadS tChar))),
     [([PVar "p"],
       ap [evar "readParen", econst falseCfun, elambda ([PVar "r"],
                                                        eCompFrom (PCon tup2Cfun [PCon consCfun [PLit (LitChar '\''), PVar "s"], PVar "t"]) (ap [evar "lex", evar "r"])
                                                        (eCompFrom (PCon tup2Cfun [PVar "c", PLit (LitStr "'")]) (ap [evar "readLitChar", evar "s"])
                                                         (eListRet (ap [econst tup2Cfun, evar "c", evar "t"]))))])]),
    ("v439",
     Just (Forall []
            ([isIn1 cRead tChar] :=>
             tReadS (TAp tList tChar))),
     [([],
       elet [[("readl",
               Nothing,
               [([PCon consCfun [PLit (LitChar '"'), PVar "s"]],
                 eCons (ap [econst tup2Cfun, elit (LitStr ""), evar "s"])
                 eNil),
                ([PCon consCfun [PLit (LitChar '\\'), PCon consCfun [PLit (LitChar '&'), PVar "s"]]],
                 ap [evar "readl", evar "s"]),
                ([PVar "s"],
                 eCompFrom (PCon tup2Cfun [PVar "c", PVar "t"]) (ap [evar "readLitChar", evar "s"])
                 (eCompFrom (PCon tup2Cfun [PVar "cs", PVar "u"]) (ap [evar "readl", evar "t"])
                  (eListRet (ap [econst tup2Cfun, ap [econst consCfun, evar "c", evar "cs"], evar "u"]))))])]]
            (ap [evar "readParen", econst falseCfun, elambda ([PVar "r"],
                                                              eCompFrom (PCon tup2Cfun [PCon consCfun [PLit (LitChar '"'), PVar "s"], PVar "t"]) (ap [evar "lex", evar "r"])
                                                              (eCompFrom (PCon tup2Cfun [PVar "l", PWildcard]) (ap [evar "readl", evar "s"])
                                                               (eListRet (ap [econst tup2Cfun, evar "l", evar "t"]))))]))]),
    ("v442",
     Just (Forall []
            ([isIn1 cShow tChar] :=>
             (tInt `fn` tChar `fn` tShowS))),
     [([PVar "p", PLit (LitChar '\'')],
       ap [evar "showString", elit (LitStr "'\\\\''")]),
      ([PVar "p", PVar "c"],
       ap [evar ".", ap [evar "showChar", elit (LitChar '\'')], ap [evar ".", ap [evar "showLitChar", evar "c"], ap [evar "showChar", elit (LitChar '\'')]]])]),
    ("v443",
     Just (Forall []
            ([isIn1 cShow tChar] :=>
             (TAp tList tChar `fn` tShowS))),
     [([PVar "cs"],
       elet [[("showl",
               Nothing,
               [([PLit (LitStr "")],
                 ap [evar "showChar", elit (LitChar '"')]),
                ([PCon consCfun [PLit (LitChar '"'), PVar "cs"]],
                 ap [evar ".", ap [evar "showString", elit (LitStr "\\\\\"")], ap [evar "showl", evar "cs"]]),
                ([PCon consCfun [PVar "c", PVar "cs"]],
                 ap [evar ".", ap [evar "showLitChar", evar "c"], ap [evar "showl", evar "cs"]])])]]
            (ap [evar ".", ap [evar "showChar", elit (LitChar '"')], ap [evar "showl", evar "cs"]]))]),
    ("v445",
     Just (Forall []
            ([isIn1 cBounded tChar] :=>
             tChar)),
     [([],
       elit (LitChar '\NUL'))]),
    ("v446",
     Just (Forall []
            ([isIn1 cBounded tChar] :=>
             tChar)),
     [([],
       elit (LitChar 'ÿ'))]),
    ("v448",
     Just (Forall [Star, Star]
            ([isIn1 cFunctor tMaybe] :=>
             ((TGen 0 `fn` TGen 1) `fn` TAp tMaybe (TGen 0) `fn` TAp tMaybe (TGen 1)))),
     [([PVar "f", PCon nothingCfun []],
       econst nothingCfun),
      ([PVar "f", PCon justCfun [PVar "x"]],
       ap [econst justCfun, ap [evar "f", evar "x"]])]),
    ("v450",
     Just (Forall [Star]
            ([isIn1 cMonad tMaybe] :=>
             (TGen 0 `fn` TAp tMaybe (TGen 0)))),
     [([],
       econst justCfun)]),
    ("v451",
     Just (Forall [Star, Star]
            ([isIn1 cMonad tMaybe] :=>
             (TAp tMaybe (TGen 0) `fn` (TGen 0 `fn` TAp tMaybe (TGen 1)) `fn` TAp tMaybe (TGen 1)))),
     [([PCon justCfun [PVar "x"], PVar "k"],
       ap [evar "k", evar "x"]),
      ([PCon nothingCfun [], PVar "k"],
       econst nothingCfun)]),
    ("v453",
     Just (Forall [Star]
            ([isIn1 cMonad tMaybe] :=>
             (tString `fn` TAp tMaybe (TGen 0)))),
     [([PVar "s"],
       econst nothingCfun)]),
    ("v455",
     Just (Forall [Star]
            ([isIn1 cEq (TGen 0)] :=>
             (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tBool))),
     [([PCon nilCfun [], PCon nilCfun []],
       econst trueCfun),
      ([PCon consCfun [PVar "x", PVar "xs"], PCon consCfun [PVar "y", PVar "ys"]],
       ap [evar "&&", ap [econst eqMfun, evar "x", evar "y"], ap [econst eqMfun, evar "xs", evar "ys"]]),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v459",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tOrdering))),
     [([PCon nilCfun [], PCon consCfun [PWildcard, PWildcard]],
       econst lTCfun),
      ([PCon nilCfun [], PCon nilCfun []],
       econst eQCfun),
      ([PCon consCfun [PWildcard, PWildcard], PCon nilCfun []],
       econst gTCfun),
      ([PCon consCfun [PVar "x", PVar "xs"], PCon consCfun [PVar "y", PVar "ys"]],
       ap [evar "primCompAux", evar "x", evar "y", ap [econst compareMfun, evar "xs", evar "ys"]])]),
    ("v467",
     Just (Forall [Star, Star]
            ([isIn1 cFunctor tList] :=>
             ((TGen 0 `fn` TGen 1) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))),
     [([],
       evar "map")]),
    ("v469",
     Just (Forall [Star]
            ([isIn1 cMonad tList] :=>
             (TGen 0 `fn` TAp tList (TGen 0)))),
     [([PVar "x"],
       eCons (evar "x")
       eNil)]),
    ("v470",
     Just (Forall [Star, Star]
            ([isIn1 cMonad tList] :=>
             (TAp tList (TGen 0) `fn` (TGen 0 `fn` TAp tList (TGen 1)) `fn` TAp tList (TGen 1)))),
     [([PCon consCfun [PVar "x", PVar "xs"], PVar "f"],
       ap [evar "++", ap [evar "f", evar "x"], ap [econst mbindMfun, evar "xs", evar "f"]]),
      ([PCon nilCfun [], PVar "f"],
       econst nilCfun)]),
    ("v472",
     Just (Forall [Star]
            ([isIn1 cMonad tList] :=>
             (tString `fn` TAp tList (TGen 0)))),
     [([PVar "s"],
       econst nilCfun)]),
    ("v474",
     Just (Forall [Star]
            ([isIn1 cRead (TGen 0)] :=>
             (tInt `fn` tReadS (TAp tList (TGen 0))))),
     [([PVar "p"],
       econst readListMfun)]),
    ("v478",
     Just (Forall [Star]
            ([isIn1 cShow (TGen 0)] :=>
             (tInt `fn` TAp tList (TGen 0) `fn` tShowS))),
     [([PVar "p"],
       econst showListMfun)]),
    ("v482",
     Just (Forall [Star, Star]
            ([isIn1 cShow (TGen 0 `fn` TGen 1)] :=>
             (tInt `fn` (TGen 0 `fn` TGen 1) `fn` tShowS))),
     [([PVar "p", PVar "f"],
       ap [evar "showString", elit (LitStr "<<function>>")])]),
    ("v485",
     Just (Forall [Star, Star, Star]
            ([isIn1 cFunctor (TAp tArrow (TGen 0))] :=>
             ((TGen 1 `fn` TGen 2) `fn` (TGen 0 `fn` TGen 1) `fn` TGen 0 `fn` TGen 2))),
     [([],
       evar ".")]),
    ("v487",
     Just (Forall []
            ([isIn1 cEq tInt] :=>
             (tInt `fn` tInt `fn` tBool))),
     [([],
       evar "primEqInt")]),
    ("v490",
     Just (Forall []
            ([isIn1 cEq tInteger] :=>
             (tInteger `fn` tInteger `fn` tBool))),
     [([],
       evar "primEqInteger")]),
    ("v494",
     Just (Forall []
            ([isIn1 cOrd tInt] :=>
             (tInt `fn` tInt `fn` tOrdering))),
     [([],
       evar "primCmpInt")]),
    ("v503",
     Just (Forall []
            ([isIn1 cOrd tInteger] :=>
             (tInteger `fn` tInteger `fn` tOrdering))),
     [([],
       evar "primCmpInteger")]),
    ("v513",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primPlusInt")]),
    ("v514",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primMinusInt")]),
    ("v515",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primMulInt")]),
    ("v516",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt))),
     [([],
       evar "primNegInt")]),
    ("v517",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt))),
     [([],
       evar "absReal")]),
    ("v518",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt))),
     [([],
       evar "signumReal")]),
    ("v519",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInteger `fn` tInt))),
     [([],
       evar "primIntegerToInt")]),
    ("v520",
     Just (Forall []
            ([isIn1 cNum tInt] :=>
             (tInt `fn` tInt))),
     [([PVar "x"],
       evar "x")]),
    ("v522",
     Just (Forall []
            ([isIn1 cBounded tInt] :=>
             tInt)),
     [([],
       evar "primMinInt")]),
    ("v523",
     Just (Forall []
            ([isIn1 cBounded tInt] :=>
             tInt)),
     [([],
       evar "primMaxInt")]),
    ("v527",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger `fn` tInteger))),
     [([],
       evar "primPlusInteger")]),
    ("v528",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger `fn` tInteger))),
     [([],
       evar "primMinusInteger")]),
    ("v529",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger `fn` tInteger))),
     [([],
       evar "primMulInteger")]),
    ("v530",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger))),
     [([],
       evar "primNegInteger")]),
    ("v531",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger))),
     [([],
       evar "absReal")]),
    ("v532",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger))),
     [([],
       evar "signumReal")]),
    ("v533",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInteger `fn` tInteger))),
     [([PVar "x"],
       evar "x")]),
    ("v534",
     Just (Forall []
            ([isIn1 cNum tInteger] :=>
             (tInt `fn` tInteger))),
     [([],
       evar "primIntToInteger")]),
    ("v538",
     Just (Forall []
            ([isIn1 cReal tInt] :=>
             (tInt `fn` tRational))),
     [([PVar "x"],
       ap [evar "%", ap [econst toIntegerMfun, evar "x"], elit (LitInt 1)])]),
    ("v542",
     Just (Forall []
            ([isIn1 cReal tInteger] :=>
             (tInteger `fn` tRational))),
     [([PVar "x"],
       ap [evar "%", evar "x", elit (LitInt 1)])]),
    ("v546",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primQuotInt")]),
    ("v547",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primRemInt")]),
    ("v548",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primDivInt")]),
    ("v549",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt `fn` tInt))),
     [([],
       evar "primModInt")]),
    ("v550",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt `fn` TAp (TAp tTuple2 tInt) tInt))),
     [([],
       evar "primQrmInt")]),
    ("v552",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tBool))),
     [([],
       evar "primEvenInt")]),
    ("v554",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInteger))),
     [([],
       evar "primIntToInteger")]),
    ("v555",
     Just (Forall []
            ([isIn1 cIntegral tInt] :=>
             (tInt `fn` tInt))),
     [([PVar "x"],
       evar "x")]),
    ("v563",
     Just (Forall []
            ([isIn1 cIntegral tInteger] :=>
             (tInteger `fn` tInteger `fn` TAp (TAp tTuple2 tInteger) tInteger))),
     [([],
       evar "primQrmInteger")]),
    ("v565",
     Just (Forall []
            ([isIn1 cIntegral tInteger] :=>
             (tInteger `fn` tBool))),
     [([],
       evar "primEvenInteger")]),
    ("v567",
     Just (Forall []
            ([isIn1 cIntegral tInteger] :=>
             (tInteger `fn` tInteger))),
     [([PVar "x"],
       evar "x")]),
    ("v568",
     Just (Forall []
            ([isIn1 cIntegral tInteger] :=>
             (tInteger `fn` tInt))),
     [([],
       evar "primIntegerToInt")]),
    ("v571",
     Just (Forall []
            ([isIn1 cIx tInt] :=>
             (TAp (TAp tTuple2 tInt) tInt `fn` TAp tList tInt))),
     [([PCon tup2Cfun [PVar "m", PVar "n"]],
       ap [econst enumFromToMfun, evar "m", evar "n"])]),
    ("v572",
     Just (Forall []
            ([isIn1 cIx tInt] :=>
             (TAp (TAp tTuple2 tInt) tInt `fn` tInt `fn` tInt))),
     [([PAs "b" (PCon tup2Cfun [PVar "m", PVar "n"]), PVar "i"],
       eguarded [(ap [econst inRangeMfun, evar "b", evar "i"],
                  ap [econst minusMfun, evar "i", evar "m"]),
                 (evar "otherwise",
                  ap [evar "error", elit (LitStr "index: Index out of range")])])]),
    ("v573",
     Just (Forall []
            ([isIn1 cIx tInt] :=>
             (TAp (TAp tTuple2 tInt) tInt `fn` tInt `fn` tBool))),
     [([PCon tup2Cfun [PVar "m", PVar "n"], PVar "i"],
       ap [evar "&&", ap [econst leMfun, evar "m", evar "i"], ap [econst leMfun, evar "i", evar "n"]])]),
    ("v577",
     Just (Forall []
            ([isIn1 cIx tInteger] :=>
             (TAp (TAp tTuple2 tInteger) tInteger `fn` TAp tList tInteger))),
     [([PCon tup2Cfun [PVar "m", PVar "n"]],
       ap [econst enumFromToMfun, evar "m", evar "n"])]),
    ("v578",
     Just (Forall []
            ([isIn1 cIx tInteger] :=>
             (TAp (TAp tTuple2 tInteger) tInteger `fn` tInteger `fn` tInt))),
     [([PAs "b" (PCon tup2Cfun [PVar "m", PVar "n"]), PVar "i"],
       eguarded [(ap [econst inRangeMfun, evar "b", evar "i"],
                  ap [econst fromIntegerMfun, ap [econst minusMfun, evar "i", evar "m"]]),
                 (evar "otherwise",
                  ap [evar "error", elit (LitStr "index: Index out of range")])])]),
    ("v579",
     Just (Forall []
            ([isIn1 cIx tInteger] :=>
             (TAp (TAp tTuple2 tInteger) tInteger `fn` tInteger `fn` tBool))),
     [([PCon tup2Cfun [PVar "m", PVar "n"], PVar "i"],
       ap [evar "&&", ap [econst leMfun, evar "m", evar "i"], ap [econst leMfun, evar "i", evar "n"]])]),
    ("v584",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` tInt))),
     [([],
       evar "id")]),
    ("v585",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` tInt))),
     [([],
       evar "id")]),
    ("v586",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` TAp tList tInt))),
     [([],
       evar "numericEnumFrom")]),
    ("v587",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` tInt `fn` TAp tList tInt))),
     [([],
       evar "numericEnumFromThen")]),
    ("v588",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` tInt `fn` TAp tList tInt))),
     [([],
       evar "numericEnumFromTo")]),
    ("v589",
     Just (Forall []
            ([isIn1 cEnum tInt] :=>
             (tInt `fn` tInt `fn` tInt `fn` TAp tList tInt))),
     [([],
       evar "numericEnumFromThenTo")]),
    ("v593",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInt `fn` tInteger))),
     [([],
       evar "primIntToInteger")]),
    ("v594",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInteger `fn` tInt))),
     [([],
       evar "primIntegerToInt")]),
    ("v595",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInteger `fn` TAp tList tInteger))),
     [([],
       evar "numericEnumFrom")]),
    ("v596",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInteger `fn` tInteger `fn` TAp tList tInteger))),
     [([],
       evar "numericEnumFromThen")]),
    ("v597",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInteger `fn` tInteger `fn` TAp tList tInteger))),
     [([],
       evar "numericEnumFromTo")]),
    ("v598",
     Just (Forall []
            ([isIn1 cEnum tInteger] :=>
             (tInteger `fn` tInteger `fn` tInteger `fn` TAp tList tInteger))),
     [([],
       evar "numericEnumFromThenTo")]),
    ("v600",
     Just (Forall []
            ([isIn1 cRead tInt] :=>
             (tInt `fn` tReadS tInt))),
     [([PVar "p"],
       ap [evar "readSigned", evar "readDec"])]),
    ("v604",
     Just (Forall []
            ([isIn1 cShow tInt] :=>
             (tInt `fn` tInt `fn` tShowS))),
     [([],
       evar "primShowsInt")]),
    ("v607",
     Just (Forall []
            ([isIn1 cRead tInteger] :=>
             (tInt `fn` tReadS tInteger))),
     [([PVar "p"],
       ap [evar "readSigned", evar "readDec"])]),
    ("v611",
     Just (Forall []
            ([isIn1 cShow tInteger] :=>
             (tInt `fn` tInteger `fn` tShowS))),
     [([],
       evar "primShowsInteger")]),
    ("v614",
     Just (Forall []
            ([isIn1 cEq tFloat] :=>
             (tFloat `fn` tFloat `fn` tBool))),
     [([],
       evar "primEqFloat")]),
    ("v617",
     Just (Forall []
            ([isIn1 cEq tDouble] :=>
             (tDouble `fn` tDouble `fn` tBool))),
     [([],
       evar "primEqDouble")]),
    ("v621",
     Just (Forall []
            ([isIn1 cOrd tFloat] :=>
             (tFloat `fn` tFloat `fn` tOrdering))),
     [([],
       evar "primCmpFloat")]),
    ("v630",
     Just (Forall []
            ([isIn1 cOrd tDouble] :=>
             (tDouble `fn` tDouble `fn` tOrdering))),
     [([],
       evar "primCmpDouble")]),
    ("v640",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat `fn` tFloat))),
     [([],
       evar "primPlusFloat")]),
    ("v641",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat `fn` tFloat))),
     [([],
       evar "primMinusFloat")]),
    ("v642",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat `fn` tFloat))),
     [([],
       evar "primMulFloat")]),
    ("v643",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primNegFloat")]),
    ("v644",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "absReal")]),
    ("v645",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "signumReal")]),
    ("v646",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tInteger `fn` tFloat))),
     [([],
       evar "primIntegerToFloat")]),
    ("v647",
     Just (Forall []
            ([isIn1 cNum tFloat] :=>
             (tInt `fn` tFloat))),
     [([],
       evar "primIntToFloat")]),
    ("v651",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble `fn` tDouble))),
     [([],
       evar "primPlusDouble")]),
    ("v652",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble `fn` tDouble))),
     [([],
       evar "primMinusDouble")]),
    ("v653",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble `fn` tDouble))),
     [([],
       evar "primMulDouble")]),
    ("v654",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primNegDouble")]),
    ("v655",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "absReal")]),
    ("v656",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "signumReal")]),
    ("v657",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tInteger `fn` tDouble))),
     [([],
       evar "primIntegerToDouble")]),
    ("v658",
     Just (Forall []
            ([isIn1 cNum tDouble] :=>
             (tInt `fn` tDouble))),
     [([],
       evar "primIntToDouble")]),
    ("v662",
     Just (Forall []
            ([isIn1 cReal tFloat] :=>
             (tFloat `fn` tRational))),
     [([],
       evar "floatToRational")]),
    ("v666",
     Just (Forall []
            ([isIn1 cReal tDouble] :=>
             (tDouble `fn` tRational))),
     [([],
       evar "doubleToRational")]),
    ("v669",
     Just (Forall []
            ([isIn1 cFractional tFloat] :=>
             (tFloat `fn` tFloat `fn` tFloat))),
     [([],
       evar "primDivFloat")]),
    ("v671",
     Just (Forall []
            ([isIn1 cFractional tFloat] :=>
             (tRational `fn` tFloat))),
     [([],
       evar "primRationalToFloat")]),
    ("v672",
     Just (Forall []
            ([isIn1 cFractional tFloat] :=>
             (tDouble `fn` tFloat))),
     [([],
       evar "doubleToFloat")]),
    ("v675",
     Just (Forall []
            ([isIn1 cFractional tDouble] :=>
             (tDouble `fn` tDouble `fn` tDouble))),
     [([],
       evar "primDivDouble")]),
    ("v677",
     Just (Forall []
            ([isIn1 cFractional tDouble] :=>
             (tRational `fn` tDouble))),
     [([],
       evar "primRationalToDouble")]),
    ("v678",
     Just (Forall []
            ([isIn1 cFractional tDouble] :=>
             (tDouble `fn` tDouble))),
     [([PVar "x"],
       evar "x")]),
    ("v682",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primExpFloat")]),
    ("v683",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primLogFloat")]),
    ("v684",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primSqrtFloat")]),
    ("v687",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primSinFloat")]),
    ("v688",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primCosFloat")]),
    ("v689",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primTanFloat")]),
    ("v690",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primAsinFloat")]),
    ("v691",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primAcosFloat")]),
    ("v692",
     Just (Forall []
            ([isIn1 cFloating tFloat] :=>
             (tFloat `fn` tFloat))),
     [([],
       evar "primAtanFloat")]),
    ("v702",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primExpDouble")]),
    ("v703",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primLogDouble")]),
    ("v704",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primSqrtDouble")]),
    ("v707",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primSinDouble")]),
    ("v708",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primCosDouble")]),
    ("v709",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primTanDouble")]),
    ("v710",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primAsinDouble")]),
    ("v711",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primAcosDouble")]),
    ("v712",
     Just (Forall []
            ([isIn1 cFloating tDouble] :=>
             (tDouble `fn` tDouble))),
     [([],
       evar "primAtanDouble")]),
    ("v722",
     Just (Forall [Star]
            ([isIn1 cRealFrac tFloat,
              isIn1 cIntegral (TGen 0)] :=>
             (tFloat `fn` TAp (TAp tTuple2 (TGen 0)) tFloat))),
     [([],
       evar "floatProperFraction")]),
    ("v730",
     Just (Forall [Star]
            ([isIn1 cRealFrac tDouble,
              isIn1 cIntegral (TGen 0)] :=>
             (tDouble `fn` TAp (TAp tTuple2 (TGen 0)) tDouble))),
     [([],
       evar "floatProperFraction")]),
    ("v738",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tInteger))),
     [([PWildcard],
       evar "primFloatRadix")]),
    ("v739",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tInt))),
     [([PWildcard],
       evar "primFloatDigits")]),
    ("v740",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` TAp (TAp tTuple2 tInt) tInt))),
     [([PWildcard],
       ap [econst tup2Cfun, evar "primFloatMinExp", evar "primFloatMaxExp"])]),
    ("v741",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` TAp (TAp tTuple2 tInteger) tInt))),
     [([],
       evar "primFloatDecode")]),
    ("v742",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tInteger `fn` tInt `fn` tFloat))),
     [([],
       evar "primFloatEncode")]),
    ("v746",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v747",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v748",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v749",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v750",
     Just (Forall []
            ([isIn1 cRealFloat tFloat] :=>
             (tFloat `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v755",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tInteger))),
     [([PWildcard],
       evar "primDoubleRadix")]),
    ("v756",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tInt))),
     [([PWildcard],
       evar "primDoubleDigits")]),
    ("v757",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` TAp (TAp tTuple2 tInt) tInt))),
     [([PWildcard],
       ap [econst tup2Cfun, evar "primDoubleMinExp", evar "primDoubleMaxExp"])]),
    ("v758",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` TAp (TAp tTuple2 tInteger) tInt))),
     [([],
       evar "primDoubleDecode")]),
    ("v759",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tInteger `fn` tInt `fn` tDouble))),
     [([],
       evar "primDoubleEncode")]),
    ("v763",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v764",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v765",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v766",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v767",
     Just (Forall []
            ([isIn1 cRealFloat tDouble] :=>
             (tDouble `fn` tBool))),
     [([PWildcard],
       econst falseCfun)]),
    ("v772",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tInt `fn` tFloat))),
     [([],
       evar "primIntToFloat")]),
    ("v773",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tFloat `fn` tInt))),
     [([],
       econst truncateMfun)]),
    ("v774",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tFloat `fn` TAp tList tFloat))),
     [([],
       evar "numericEnumFrom")]),
    ("v775",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tFloat `fn` tFloat `fn` TAp tList tFloat))),
     [([],
       evar "numericEnumFromThen")]),
    ("v776",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tFloat `fn` tFloat `fn` TAp tList tFloat))),
     [([PVar "n", PVar "m"],
       ap [evar "numericEnumFromTo", evar "n", ap [econst plusMfun, evar "m", ap [econst divideMfun, elit (LitInt 1), elit (LitInt 2)]]])]),
    ("v777",
     Just (Forall []
            ([isIn1 cEnum tFloat] :=>
             (tFloat `fn` tFloat `fn` tFloat `fn` TAp tList tFloat))),
     [([PVar "n", PVar "n'", PVar "m"],
       ap [evar "numericEnumFromThenTo", evar "n", evar "n'", ap [econst plusMfun, evar "m", ap [econst divideMfun, ap [econst minusMfun, evar "n'", evar "n"], elit (LitInt 2)]]])]),
    ("v781",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tInt `fn` tDouble))),
     [([],
       evar "primIntToDouble")]),
    ("v782",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tDouble `fn` tInt))),
     [([],
       econst truncateMfun)]),
    ("v783",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tDouble `fn` TAp tList tDouble))),
     [([],
       evar "numericEnumFrom")]),
    ("v784",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tDouble `fn` tDouble `fn` TAp tList tDouble))),
     [([],
       evar "numericEnumFromThen")]),
    ("v785",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tDouble `fn` tDouble `fn` TAp tList tDouble))),
     [([PVar "n", PVar "m"],
       ap [evar "numericEnumFromTo", evar "n", ap [econst plusMfun, evar "m", ap [econst divideMfun, elit (LitInt 1), elit (LitInt 2)]]])]),
    ("v786",
     Just (Forall []
            ([isIn1 cEnum tDouble] :=>
             (tDouble `fn` tDouble `fn` tDouble `fn` TAp tList tDouble))),
     [([PVar "n", PVar "n'", PVar "m"],
       ap [evar "numericEnumFromThenTo", evar "n", evar "n'", ap [econst plusMfun, evar "m", ap [econst divideMfun, ap [econst minusMfun, evar "n'", evar "n"], elit (LitInt 2)]]])]),
    ("v788",
     Just (Forall []
            ([isIn1 cRead tFloat] :=>
             (tInt `fn` tReadS tFloat))),
     [([PVar "p"],
       ap [evar "readSigned", evar "readFloat"])]),
    ("v792",
     Just (Forall []
            ([isIn1 cShow tFloat] :=>
             (tInt `fn` tFloat `fn` tShowS))),
     [([],
       evar "primShowsFloat")]),
    ("v795",
     Just (Forall []
            ([isIn1 cRead tDouble] :=>
             (tInt `fn` tReadS tDouble))),
     [([PVar "p"],
       ap [evar "readSigned", evar "readFloat"])]),
    ("v799",
     Just (Forall []
            ([isIn1 cShow tDouble] :=>
             (tInt `fn` tDouble `fn` tShowS))),
     [([],
       evar "primShowsDouble")]),
    ("v803",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` tOrdering))),
     [([PCon consratCfun [PVar "x", PVar "y"], PCon consratCfun [PVar "x'", PVar "y'"]],
       ap [econst compareMfun, ap [econst timesMfun, evar "x", evar "y'"], ap [econst timesMfun, evar "x'", evar "y"]])]),
    ("v813",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"], PCon consratCfun [PVar "x'", PVar "y'"]],
       ap [evar "reduce", ap [econst plusMfun, ap [econst timesMfun, evar "x", evar "y'"], ap [econst timesMfun, evar "x'", evar "y"]], ap [econst timesMfun, evar "y", evar "y'"]])]),
    ("v815",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"], PCon consratCfun [PVar "x'", PVar "y'"]],
       ap [evar "reduce", ap [econst timesMfun, evar "x", evar "x'"], ap [econst timesMfun, evar "y", evar "y'"]])]),
    ("v816",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       ap [econst consratCfun, ap [econst negateMfun, evar "x"], evar "y"])]),
    ("v817",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       ap [econst consratCfun, ap [econst absMfun, evar "x"], evar "y"])]),
    ("v818",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       ap [econst consratCfun, ap [econst signumMfun, evar "x"], elit (LitInt 1)])]),
    ("v819",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tInteger `fn` TAp tRatio (TGen 0)))),
     [([PVar "x"],
       ap [econst consratCfun, ap [econst fromIntegerMfun, evar "x"], elit (LitInt 1)])]),
    ("v820",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tInt `fn` TAp tRatio (TGen 0)))),
     [([],
       evar "intToRatio")]),
    ("v824",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` tRational))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       ap [econst consratCfun, ap [econst toIntegerMfun, evar "x"], ap [econst toIntegerMfun, evar "y"]])]),
    ("v827",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"], PCon consratCfun [PVar "x'", PVar "y'"]],
       ap [evar "%", ap [econst timesMfun, evar "x", evar "y'"], ap [econst timesMfun, evar "y", evar "x'"]])]),
    ("v828",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       eif (ap [econst ltMfun, evar "x", elit (LitInt 0)])
           (ap [econst consratCfun, ap [econst negateMfun, evar "y"], ap [econst negateMfun, evar "x"]])
           (ap [econst consratCfun, evar "y", evar "x"]))]),
    ("v829",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tRational `fn` TAp tRatio (TGen 0)))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       ap [econst consratCfun, ap [econst fromIntegerMfun, evar "x"], ap [econst fromIntegerMfun, evar "y"]])]),
    ("v830",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tDouble `fn` TAp tRatio (TGen 0)))),
     [([],
       evar "doubleToRatio")]),
    ("v834",
     Just (Forall [Star, Star]
            ([isIn1 cIntegral (TGen 0),
              isIn1 cIntegral (TGen 1)] :=>
             (TAp tRatio (TGen 0) `fn` TAp (TAp tTuple2 (TGen 1)) (TAp tRatio (TGen 0))))),
     [([PCon consratCfun [PVar "x", PVar "y"]],
       elet [[("v835",
               Nothing,
               [([], ap [econst quotRemMfun, evar "x", evar "y"])]),
              ("q",
               Nothing,
               [([], ecase (evar "v835") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "q")])]),
              ("r",
               Nothing,
               [([], ecase (evar "v835") [(PCon tup2Cfun [PVar "q", PVar "r"], evar "r")])])]]
            (ap [econst tup2Cfun, ap [evar "fromIntegral", evar "q"], ap [econst consratCfun, evar "r", evar "y"]]))]),
    ("v843",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tInt `fn` TAp tRatio (TGen 0)))),
     [([],
       econst fromIntMfun)]),
    ("v844",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` tInt))),
     [([],
       econst truncateMfun)]),
    ("v845",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tList (TAp tRatio (TGen 0))))),
     [([],
       evar "numericEnumFrom")]),
    ("v846",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` TAp tList (TAp tRatio (TGen 0))))),
     [([],
       evar "numericEnumFromThen")]),
    ("v850",
     Just (Forall [Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cIntegral (TGen 0)] :=>
             (tInt `fn` tReadS (TAp tRatio (TGen 0))))),
     [([PVar "p"],
       ap [evar "readParen", ap [econst gtMfun, evar "p", elit (LitInt 7)], elambda ([PVar "r"],
                                                                                     eCompFrom (PCon tup2Cfun [PVar "x", PVar "s"]) (ap [evar "reads", evar "r"])
                                                                                     (eCompFrom (PCon tup2Cfun [PLit (LitStr "%"), PVar "t"]) (ap [evar "lex", evar "s"])
                                                                                      (eCompFrom (PCon tup2Cfun [PVar "y", PVar "u"]) (ap [evar "reads", evar "t"])
                                                                                       (eListRet (ap [econst tup2Cfun, ap [evar "%", evar "x", evar "y"], evar "u"])))))])]),
    ("v854",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (tInt `fn` TAp tRatio (TGen 0) `fn` tShowS))),
     [([PVar "p", PCon consratCfun [PVar "x", PVar "y"]],
       ap [evar "showParen", ap [econst gtMfun, evar "p", elit (LitInt 7)], ap [evar ".", ap [evar "shows", evar "x"], ap [evar ".", ap [evar "showString", elit (LitStr " % ")], ap [evar "shows", evar "y"]]]])]),
    ("v858",
     Just (Forall [Star]
            ([isIn1 cShow (TAp tIO (TGen 0))] :=>
             (tInt `fn` TAp tIO (TGen 0) `fn` tShowS))),
     [([PVar "p", PVar "f"],
       ap [evar "showString", elit (LitStr "<<IO action>>")])]),
    ("v861",
     Just (Forall [Star, Star]
            ([isIn1 cFunctor tIO] :=>
             ((TGen 0 `fn` TGen 1) `fn` TAp tIO (TGen 0) `fn` TAp tIO (TGen 1)))),
     [([PVar "f", PVar "x"],
       ap [econst mbindMfun, evar "x", ap [evar ".", econst returnMfun, evar "f"]])]),
    ("v863",
     Just (Forall [Star]
            ([isIn1 cMonad tIO] :=>
             (TGen 0 `fn` TAp tIO (TGen 0)))),
     [([],
       evar "primretIO")]),
    ("v864",
     Just (Forall [Star, Star]
            ([isIn1 cMonad tIO] :=>
             (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1)))),
     [([],
       evar "primbindIO")]),
    ("v869",
     Just (Forall []
            ([isIn1 cShow tIOError] :=>
             (tInt `fn` tIOError `fn` tShowS))),
     [([PVar "p", PVar "x"],
       ap [evar "showString", ap [evar "ioeGetErrorString", evar "x"]])]),
    ("v872",
     Just (Forall []
            ([isIn1 cBounded tBool] :=>
             tBool)),
     [([],
       econst falseCfun)]),
    ("v873",
     Just (Forall []
            ([isIn1 cBounded tBool] :=>
             tBool)),
     [([],
       econst trueCfun)]),
    ("v876",
     Just (Forall []
            ([isIn1 cShow tBool] :=>
             (tInt `fn` tBool `fn` tShowS))),
     [([PVar "v85", PCon falseCfun []],
       ap [evar "++", elit (LitStr "False")]),
      ([PVar "v85", PCon trueCfun []],
       ap [evar "++", elit (LitStr "True")])]),
    ("v879",
     Just (Forall []
            ([isIn1 cRead tBool] :=>
             (tInt `fn` tReadS tBool))),
     [([PVar "v87", PVar "v88"],
       ap [evar "++", ap [evar "readParen", econst falseCfun, elambda ([PVar "v89"],
                                                                       eCompFrom (PCon tup2Cfun [PLit (LitStr "False"), PVar "v90"]) (ap [evar "lex", evar "v89"])
                                                                       (eListRet (ap [econst tup2Cfun, econst falseCfun, evar "v90"]))), evar "v88"], ap [evar "readParen", econst falseCfun, elambda ([PVar "v91"],
                                                                                                                                                                                                       eCompFrom (PCon tup2Cfun [PLit (LitStr "True"), PVar "v92"]) (ap [evar "lex", evar "v91"])
                                                                                                                                                                                                       (eListRet (ap [econst tup2Cfun, econst trueCfun, evar "v92"]))), evar "v88"]])]),
    ("v884",
     Just (Forall []
            ([isIn1 cEnum tBool] :=>
             (tInt `fn` tBool))),
     [([],
       ap [evar "_ToEnum", econst falseCfun])]),
    ("v885",
     Just (Forall []
            ([isIn1 cEnum tBool] :=>
             (tBool `fn` tInt))),
     [([],
       evar "_FrEnum")]),
    ("v886",
     Just (Forall []
            ([isIn1 cEnum tBool] :=>
             (tBool `fn` TAp tList tBool))),
     [([],
       evar "_From")]),
    ("v887",
     Just (Forall []
            ([isIn1 cEnum tBool] :=>
             (tBool `fn` tBool `fn` TAp tList tBool))),
     [([],
       evar "_FromThen")]),
    ("v888",
     Just (Forall []
            ([isIn1 cEnum tBool] :=>
             (tBool `fn` tBool `fn` TAp tList tBool))),
     [([],
       evar "_FromTo")]),
    ("v892",
     Just (Forall []
            ([isIn1 cIx tBool] :=>
             (TAp (TAp tTuple2 tBool) tBool `fn` TAp tList tBool))),
     [([],
       evar "_range")]),
    ("v893",
     Just (Forall []
            ([isIn1 cIx tBool] :=>
             (TAp (TAp tTuple2 tBool) tBool `fn` tBool `fn` tInt))),
     [([],
       evar "_index")]),
    ("v894",
     Just (Forall []
            ([isIn1 cIx tBool] :=>
             (TAp (TAp tTuple2 tBool) tBool `fn` tBool `fn` tBool))),
     [([],
       evar "_inRange")]),
    ("v898",
     Just (Forall []
            ([isIn1 cOrd tBool] :=>
             (tBool `fn` tBool `fn` tOrdering))),
     [([],
       evar "_concmp")]),
    ("v906",
     Just (Forall []
            ([isIn1 cEq tBool] :=>
             (tBool `fn` tBool `fn` tBool))),
     [([PCon falseCfun [], PCon falseCfun []],
       econst trueCfun),
      ([PCon trueCfun [], PCon trueCfun []],
       econst trueCfun),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v910",
     Just (Forall [Star]
            ([isIn1 cShow (TGen 0)] :=>
             (tInt `fn` TAp tMaybe (TGen 0) `fn` tShowS))),
     [([PVar "v85", PCon nothingCfun []],
       ap [evar "++", elit (LitStr "Nothing")]),
      ([PVar "v98", PCon justCfun [PVar "v85"]],
       ap [evar "showParen", ap [econst leMfun, elit (LitInt 10), evar "v98"], ap [evar ".", ap [evar "++", elit (LitStr "Just")], ap [evar ".", ap [econst consCfun, elit (LitChar ' ')], ap [econst showsPrecMfun, elit (LitInt 10), evar "v85"]]]])]),
    ("v913",
     Just (Forall [Star]
            ([isIn1 cRead (TGen 0)] :=>
             (tInt `fn` tReadS (TAp tMaybe (TGen 0))))),
     [([PVar "v100", PVar "v101"],
       ap [evar "++", ap [evar "readParen", econst falseCfun, elambda ([PVar "v102"],
                                                                       eCompFrom (PCon tup2Cfun [PLit (LitStr "Nothing"), PVar "v103"]) (ap [evar "lex", evar "v102"])
                                                                       (eListRet (ap [econst tup2Cfun, econst nothingCfun, evar "v103"]))), evar "v101"], ap [evar "readParen", ap [econst gtMfun, evar "v100", elit (LitInt 9)], elambda ([PVar "v104"],
                                                                                                                                                                                                                                           eCompFrom (PCon tup2Cfun [PLit (LitStr "Just"), PVar "v105"]) (ap [evar "lex", evar "v104"])
                                                                                                                                                                                                                                           (eCompFrom (PCon tup2Cfun [PVar "v106", PVar "v107"]) (ap [econst readsPrecMfun, elit (LitInt 10), evar "v105"])
                                                                                                                                                                                                                                            (eListRet (ap [econst tup2Cfun, ap [econst justCfun, evar "v106"], evar "v107"])))), evar "v101"]])]),
    ("v917",
     Just (Forall [Star]
            ([isIn1 cOrd (TGen 0)] :=>
             (TAp tMaybe (TGen 0) `fn` TAp tMaybe (TGen 0) `fn` tOrdering))),
     [([PCon nothingCfun [], PCon nothingCfun []],
       econst eQCfun),
      ([PCon justCfun [PVar "v85"], PCon justCfun [PVar "v98"]],
       ap [econst compareMfun, evar "v85", evar "v98"]),
      ([PVar "v109", PVar "v110"],
       ap [evar "_concmp", evar "v109", evar "v110"])]),
    ("v925",
     Just (Forall [Star]
            ([isIn1 cEq (TGen 0)] :=>
             (TAp tMaybe (TGen 0) `fn` TAp tMaybe (TGen 0) `fn` tBool))),
     [([PCon nothingCfun [], PCon nothingCfun []],
       econst trueCfun),
      ([PCon justCfun [PVar "v85"], PCon justCfun [PVar "v98"]],
       ap [econst eqMfun, evar "v85", evar "v98"]),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v929",
     Just (Forall [Star, Star]
            ([isIn1 cShow (TGen 0),
              isIn1 cShow (TGen 1)] :=>
             (tInt `fn` TAp (TAp tEither (TGen 0)) (TGen 1) `fn` tShowS))),
     [([PVar "v98", PCon leftCfun [PVar "v85"]],
       ap [evar "showParen", ap [econst leMfun, elit (LitInt 10), evar "v98"], ap [evar ".", ap [evar "++", elit (LitStr "Left")], ap [evar ".", ap [econst consCfun, elit (LitChar ' ')], ap [econst showsPrecMfun, elit (LitInt 10), evar "v85"]]]]),
      ([PVar "v98", PCon rightCfun [PVar "v85"]],
       ap [evar "showParen", ap [econst leMfun, elit (LitInt 10), evar "v98"], ap [evar ".", ap [evar "++", elit (LitStr "Right")], ap [evar ".", ap [econst consCfun, elit (LitChar ' ')], ap [econst showsPrecMfun, elit (LitInt 10), evar "v85"]]]])]),
    ("v932",
     Just (Forall [Star, Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cRead (TGen 1)] :=>
             (tInt `fn` tReadS (TAp (TAp tEither (TGen 0)) (TGen 1))))),
     [([PVar "v114", PVar "v115"],
       ap [evar "++", ap [evar "readParen", ap [econst gtMfun, evar "v114", elit (LitInt 9)], elambda ([PVar "v116"],
                                                                                                       eCompFrom (PCon tup2Cfun [PLit (LitStr "Left"), PVar "v117"]) (ap [evar "lex", evar "v116"])
                                                                                                       (eCompFrom (PCon tup2Cfun [PVar "v118", PVar "v119"]) (ap [econst readsPrecMfun, elit (LitInt 10), evar "v117"])
                                                                                                        (eListRet (ap [econst tup2Cfun, ap [econst leftCfun, evar "v118"], evar "v119"])))), evar "v115"], ap [evar "readParen", ap [econst gtMfun, evar "v114", elit (LitInt 9)], elambda ([PVar "v120"],
                                                                                                                                                                                                                                                                                            eCompFrom (PCon tup2Cfun [PLit (LitStr "Right"), PVar "v121"]) (ap [evar "lex", evar "v120"])
                                                                                                                                                                                                                                                                                            (eCompFrom (PCon tup2Cfun [PVar "v122", PVar "v123"]) (ap [econst readsPrecMfun, elit (LitInt 10), evar "v121"])
                                                                                                                                                                                                                                                                                             (eListRet (ap [econst tup2Cfun, ap [econst rightCfun, evar "v122"], evar "v123"])))), evar "v115"]])]),
    ("v936",
     Just (Forall [Star, Star]
            ([isIn1 cOrd (TGen 0),
              isIn1 cOrd (TGen 1)] :=>
             (TAp (TAp tEither (TGen 0)) (TGen 1) `fn` TAp (TAp tEither (TGen 0)) (TGen 1) `fn` tOrdering))),
     [([PCon leftCfun [PVar "v85"], PCon leftCfun [PVar "v98"]],
       ap [econst compareMfun, evar "v85", evar "v98"]),
      ([PCon rightCfun [PVar "v85"], PCon rightCfun [PVar "v98"]],
       ap [econst compareMfun, evar "v85", evar "v98"]),
      ([PVar "v125", PVar "v126"],
       ap [evar "_concmp", evar "v125", evar "v126"])]),
    ("v944",
     Just (Forall [Star, Star]
            ([isIn1 cEq (TGen 0),
              isIn1 cEq (TGen 1)] :=>
             (TAp (TAp tEither (TGen 0)) (TGen 1) `fn` TAp (TAp tEither (TGen 0)) (TGen 1) `fn` tBool))),
     [([PCon leftCfun [PVar "v85"], PCon leftCfun [PVar "v98"]],
       ap [econst eqMfun, evar "v85", evar "v98"]),
      ([PCon rightCfun [PVar "v85"], PCon rightCfun [PVar "v98"]],
       ap [econst eqMfun, evar "v85", evar "v98"]),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v947",
     Just (Forall []
            ([isIn1 cBounded tOrdering] :=>
             tOrdering)),
     [([],
       econst lTCfun)]),
    ("v948",
     Just (Forall []
            ([isIn1 cBounded tOrdering] :=>
             tOrdering)),
     [([],
       econst gTCfun)]),
    ("v951",
     Just (Forall []
            ([isIn1 cShow tOrdering] :=>
             (tInt `fn` tOrdering `fn` tShowS))),
     [([PVar "v98", PCon lTCfun []],
       ap [evar "++", elit (LitStr "LT")]),
      ([PVar "v98", PCon eQCfun []],
       ap [evar "++", elit (LitStr "EQ")]),
      ([PVar "v98", PCon gTCfun []],
       ap [evar "++", elit (LitStr "GT")])]),
    ("v954",
     Just (Forall []
            ([isIn1 cRead tOrdering] :=>
             (tInt `fn` tReadS tOrdering))),
     [([PVar "v131", PVar "v132"],
       ap [evar "++", ap [evar "readParen", econst falseCfun, elambda ([PVar "v133"],
                                                                       eCompFrom (PCon tup2Cfun [PLit (LitStr "LT"), PVar "v134"]) (ap [evar "lex", evar "v133"])
                                                                       (eListRet (ap [econst tup2Cfun, econst lTCfun, evar "v134"]))), evar "v132"], ap [evar "++", ap [evar "readParen", econst falseCfun, elambda ([PVar "v135"],
                                                                                                                                                                                                                     eCompFrom (PCon tup2Cfun [PLit (LitStr "EQ"), PVar "v136"]) (ap [evar "lex", evar "v135"])
                                                                                                                                                                                                                     (eListRet (ap [econst tup2Cfun, econst eQCfun, evar "v136"]))), evar "v132"], ap [evar "readParen", econst falseCfun, elambda ([PVar "v137"],
                                                                                                                                                                                                                                                                                                                                                    eCompFrom (PCon tup2Cfun [PLit (LitStr "GT"), PVar "v138"]) (ap [evar "lex", evar "v137"])
                                                                                                                                                                                                                                                                                                                                                    (eListRet (ap [econst tup2Cfun, econst gTCfun, evar "v138"]))), evar "v132"]]])]),
    ("v959",
     Just (Forall []
            ([isIn1 cEnum tOrdering] :=>
             (tInt `fn` tOrdering))),
     [([],
       ap [evar "_ToEnum", econst lTCfun])]),
    ("v960",
     Just (Forall []
            ([isIn1 cEnum tOrdering] :=>
             (tOrdering `fn` tInt))),
     [([],
       evar "_FrEnum")]),
    ("v961",
     Just (Forall []
            ([isIn1 cEnum tOrdering] :=>
             (tOrdering `fn` TAp tList tOrdering))),
     [([],
       evar "_From")]),
    ("v962",
     Just (Forall []
            ([isIn1 cEnum tOrdering] :=>
             (tOrdering `fn` tOrdering `fn` TAp tList tOrdering))),
     [([],
       evar "_FromThen")]),
    ("v963",
     Just (Forall []
            ([isIn1 cEnum tOrdering] :=>
             (tOrdering `fn` tOrdering `fn` TAp tList tOrdering))),
     [([],
       evar "_FromTo")]),
    ("v967",
     Just (Forall []
            ([isIn1 cIx tOrdering] :=>
             (TAp (TAp tTuple2 tOrdering) tOrdering `fn` TAp tList tOrdering))),
     [([],
       evar "_range")]),
    ("v968",
     Just (Forall []
            ([isIn1 cIx tOrdering] :=>
             (TAp (TAp tTuple2 tOrdering) tOrdering `fn` tOrdering `fn` tInt))),
     [([],
       evar "_index")]),
    ("v969",
     Just (Forall []
            ([isIn1 cIx tOrdering] :=>
             (TAp (TAp tTuple2 tOrdering) tOrdering `fn` tOrdering `fn` tBool))),
     [([],
       evar "_inRange")]),
    ("v973",
     Just (Forall []
            ([isIn1 cOrd tOrdering] :=>
             (tOrdering `fn` tOrdering `fn` tOrdering))),
     [([],
       evar "_concmp")]),
    ("v981",
     Just (Forall []
            ([isIn1 cEq tOrdering] :=>
             (tOrdering `fn` tOrdering `fn` tBool))),
     [([PCon lTCfun [], PCon lTCfun []],
       econst trueCfun),
      ([PCon eQCfun [], PCon eQCfun []],
       econst trueCfun),
      ([PCon gTCfun [], PCon gTCfun []],
       econst trueCfun),
      ([PWildcard, PWildcard],
       econst falseCfun)]),
    ("v984",
     Just (Forall [Star]
            ([isIn1 cIntegral (TGen 0)] :=>
             (TAp tRatio (TGen 0) `fn` TAp tRatio (TGen 0) `fn` tBool))),
     [([PCon consratCfun [PVar "v98", PVar "v85"], PCon consratCfun [PVar "v145", PVar "v144"]],
       ap [evar "&&", ap [econst eqMfun, evar "v98", evar "v145"], ap [econst eqMfun, evar "v85", evar "v144"]])]),
    ("v988",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3),
              isIn1 cIx (TGen 4)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))) (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))))),
     [([PCon tup2Cfun [PCon tup5Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148", PVar "v144"], PCon tup5Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147", PVar "v98"]]],
       eCompFrom (PVar "v155") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"]])
       (eCompFrom (PVar "v152") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]])
        (eCompFrom (PVar "v149") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]])
         (eCompFrom (PVar "v145") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"]])
          (eCompFrom (PVar "v85") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v144", evar "v98"]])
           (eListRet (ap [econst tup5Cfun, evar "v155", evar "v152", evar "v149", evar "v145", evar "v85"])))))))]),
    ("v989",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3),
              isIn1 cIx (TGen 4)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))) (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` tInt))),
     [([PCon tup2Cfun [PCon tup5Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148", PVar "v144"], PCon tup5Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147", PVar "v98"]], PCon tup5Cfun [PVar "v155", PVar "v152", PVar "v149", PVar "v145", PVar "v85"]],
       ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v144", evar "v98"], evar "v85"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v144", evar "v98"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v148", evar "v147"], evar "v145"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]], ap [econst indexMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"]]]]]]]]])]),
    ("v990",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3),
              isIn1 cIx (TGen 4)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))) (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` tBool))),
     [([PCon tup2Cfun [PCon tup5Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148", PVar "v144"], PCon tup5Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147", PVar "v98"]], PCon tup5Cfun [PVar "v155", PVar "v152", PVar "v149", PVar "v145", PVar "v85"]],
       ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"], evar "v145"], ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v144", evar "v98"], evar "v85"]]]]])]),
    ("v993",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cRead (TGen 1),
              isIn1 cRead (TGen 2),
              isIn1 cRead (TGen 3),
              isIn1 cRead (TGen 4)] :=>
             (tInt `fn` tReadS (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4))))),
     [([PVar "v159", PVar "v160"],
       ap [elambda ([PVar "v161"],
                    eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "v164"]) (ap [evar "lex", evar "v161"])
                    (eCompFrom (PCon tup2Cfun [PVar "v163", PVar "v165"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v164"])
                     (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v167"]) (ap [evar "lex", evar "v165"])
                      (eCompFrom (PCon tup2Cfun [PVar "v166", PVar "v168"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v167"])
                       (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v170"]) (ap [evar "lex", evar "v168"])
                        (eCompFrom (PCon tup2Cfun [PVar "v169", PVar "v171"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v170"])
                         (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v173"]) (ap [evar "lex", evar "v171"])
                          (eCompFrom (PCon tup2Cfun [PVar "v172", PVar "v174"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v173"])
                           (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v176"]) (ap [evar "lex", evar "v174"])
                            (eCompFrom (PCon tup2Cfun [PVar "v175", PVar "v177"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v176"])
                             (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "v162"]) (ap [evar "lex", evar "v177"])
                              (eListRet (ap [econst tup2Cfun, ap [econst tup5Cfun, evar "v163", evar "v166", evar "v169", evar "v172", evar "v175"], evar "v162"]))))))))))))), evar "v160"])]),
    ("v997",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cShow (TGen 0),
              isIn1 cShow (TGen 1),
              isIn1 cShow (TGen 2),
              isIn1 cShow (TGen 3),
              isIn1 cShow (TGen 4)] :=>
             (tInt `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` tShowS))),
     [([PVar "v157", PCon tup5Cfun [PVar "v156", PVar "v155", PVar "v154", PVar "v153", PVar "v152"]],
       ap [evar ".", ap [econst consCfun, elit (LitChar '(')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v156"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v155"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v154"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v153"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v152"], ap [econst consCfun, elit (LitChar ')')]]]]]]]]]]])]),
    ("v1001",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cOrd (TGen 0),
              isIn1 cOrd (TGen 1),
              isIn1 cOrd (TGen 2),
              isIn1 cOrd (TGen 3),
              isIn1 cOrd (TGen 4)] :=>
             (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` tOrdering))),
     [([PCon tup5Cfun [PVar "v152", PVar "v151", PVar "v150", PVar "v149", PVar "v148"], PCon tup5Cfun [PVar "v157", PVar "v156", PVar "v155", PVar "v154", PVar "v153"]],
       ap [evar "primCompAux", evar "v152", evar "v157", ap [evar "primCompAux", evar "v151", evar "v156", ap [evar "primCompAux", evar "v150", evar "v155", ap [evar "primCompAux", evar "v149", evar "v154", ap [econst compareMfun, evar "v148", evar "v153"]]]]])]),
    ("v1009",
     Just (Forall [Star, Star, Star, Star, Star]
            ([isIn1 cEq (TGen 0),
              isIn1 cEq (TGen 1),
              isIn1 cEq (TGen 2),
              isIn1 cEq (TGen 3),
              isIn1 cEq (TGen 4)] :=>
             (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4) `fn` tBool))),
     [([PCon tup5Cfun [PVar "v152", PVar "v151", PVar "v150", PVar "v149", PVar "v148"], PCon tup5Cfun [PVar "v157", PVar "v156", PVar "v155", PVar "v154", PVar "v153"]],
       ap [evar "&&", ap [econst eqMfun, evar "v152", evar "v157"], ap [evar "&&", ap [econst eqMfun, evar "v151", evar "v156"], ap [evar "&&", ap [econst eqMfun, evar "v150", evar "v155"], ap [evar "&&", ap [econst eqMfun, evar "v149", evar "v154"], ap [econst eqMfun, evar "v148", evar "v153"]]]]])]),
    ("v1013",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))) (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) `fn` TAp tList (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))))),
     [([PCon tup2Cfun [PCon tup4Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148"], PCon tup4Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147"]]],
       eCompFrom (PVar "v155") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"]])
       (eCompFrom (PVar "v152") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]])
        (eCompFrom (PVar "v149") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]])
         (eCompFrom (PVar "v145") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"]])
          (eListRet (ap [econst tup4Cfun, evar "v155", evar "v152", evar "v149", evar "v145"]))))))]),
    ("v1014",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))) (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` tInt))),
     [([PCon tup2Cfun [PCon tup4Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148"], PCon tup4Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147"]], PCon tup4Cfun [PVar "v155", PVar "v152", PVar "v149", PVar "v145"]],
       ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v148", evar "v147"], evar "v145"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]], ap [econst indexMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"]]]]]]])]),
    ("v1015",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2),
              isIn1 cIx (TGen 3)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))) (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` tBool))),
     [([PCon tup2Cfun [PCon tup4Cfun [PVar "v157", PVar "v154", PVar "v151", PVar "v148"], PCon tup4Cfun [PVar "v156", PVar "v153", PVar "v150", PVar "v147"]], PCon tup4Cfun [PVar "v155", PVar "v152", PVar "v149", PVar "v145"]],
       ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"], ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v148", evar "v147"], evar "v145"]]]])]),
    ("v1018",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cRead (TGen 1),
              isIn1 cRead (TGen 2),
              isIn1 cRead (TGen 3)] :=>
             (tInt `fn` tReadS (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3))))),
     [([PVar "v183", PVar "v184"],
       ap [elambda ([PVar "v185"],
                    eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "v188"]) (ap [evar "lex", evar "v185"])
                    (eCompFrom (PCon tup2Cfun [PVar "v187", PVar "v189"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v188"])
                     (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v191"]) (ap [evar "lex", evar "v189"])
                      (eCompFrom (PCon tup2Cfun [PVar "v190", PVar "v192"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v191"])
                       (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v194"]) (ap [evar "lex", evar "v192"])
                        (eCompFrom (PCon tup2Cfun [PVar "v193", PVar "v195"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v194"])
                         (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v197"]) (ap [evar "lex", evar "v195"])
                          (eCompFrom (PCon tup2Cfun [PVar "v196", PVar "v198"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v197"])
                           (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "v186"]) (ap [evar "lex", evar "v198"])
                            (eListRet (ap [econst tup2Cfun, ap [econst tup4Cfun, evar "v187", evar "v190", evar "v193", evar "v196"], evar "v186"]))))))))))), evar "v184"])]),
    ("v1022",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cShow (TGen 0),
              isIn1 cShow (TGen 1),
              isIn1 cShow (TGen 2),
              isIn1 cShow (TGen 3)] :=>
             (tInt `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` tShowS))),
     [([PVar "v157", PCon tup4Cfun [PVar "v156", PVar "v155", PVar "v154", PVar "v153"]],
       ap [evar ".", ap [econst consCfun, elit (LitChar '(')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v156"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v155"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v154"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v153"], ap [econst consCfun, elit (LitChar ')')]]]]]]]]])]),
    ("v1026",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cOrd (TGen 0),
              isIn1 cOrd (TGen 1),
              isIn1 cOrd (TGen 2),
              isIn1 cOrd (TGen 3)] :=>
             (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` tOrdering))),
     [([PCon tup4Cfun [PVar "v153", PVar "v152", PVar "v151", PVar "v150"], PCon tup4Cfun [PVar "v157", PVar "v156", PVar "v155", PVar "v154"]],
       ap [evar "primCompAux", evar "v153", evar "v157", ap [evar "primCompAux", evar "v152", evar "v156", ap [evar "primCompAux", evar "v151", evar "v155", ap [econst compareMfun, evar "v150", evar "v154"]]]])]),
    ("v1034",
     Just (Forall [Star, Star, Star, Star]
            ([isIn1 cEq (TGen 0),
              isIn1 cEq (TGen 1),
              isIn1 cEq (TGen 2),
              isIn1 cEq (TGen 3)] :=>
             (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3) `fn` tBool))),
     [([PCon tup4Cfun [PVar "v153", PVar "v152", PVar "v151", PVar "v150"], PCon tup4Cfun [PVar "v157", PVar "v156", PVar "v155", PVar "v154"]],
       ap [evar "&&", ap [econst eqMfun, evar "v153", evar "v157"], ap [evar "&&", ap [econst eqMfun, evar "v152", evar "v156"], ap [evar "&&", ap [econst eqMfun, evar "v151", evar "v155"], ap [econst eqMfun, evar "v150", evar "v154"]]]])]),
    ("v1038",
     Just (Forall [Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))) (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)) `fn` TAp tList (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))))),
     [([PCon tup2Cfun [PCon tup3Cfun [PVar "v157", PVar "v154", PVar "v151"], PCon tup3Cfun [PVar "v156", PVar "v153", PVar "v150"]]],
       eCompFrom (PVar "v155") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"]])
       (eCompFrom (PVar "v152") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]])
        (eCompFrom (PVar "v149") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]])
         (eListRet (ap [econst tup3Cfun, evar "v155", evar "v152", evar "v149"])))))]),
    ("v1039",
     Just (Forall [Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))) (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)) `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` tInt))),
     [([PCon tup2Cfun [PCon tup3Cfun [PVar "v157", PVar "v154", PVar "v151"], PCon tup3Cfun [PVar "v156", PVar "v153", PVar "v150"]], PCon tup3Cfun [PVar "v155", PVar "v152", PVar "v149"]],
       ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"]], ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]], ap [econst indexMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"]]]]])]),
    ("v1040",
     Just (Forall [Star, Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1),
              isIn1 cIx (TGen 2)] :=>
             (TAp (TAp tTuple2 (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))) (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)) `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` tBool))),
     [([PCon tup2Cfun [PCon tup3Cfun [PVar "v157", PVar "v154", PVar "v151"], PCon tup3Cfun [PVar "v156", PVar "v153", PVar "v150"]], PCon tup3Cfun [PVar "v155", PVar "v152", PVar "v149"]],
       ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"], ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v151", evar "v150"], evar "v149"]]])]),
    ("v1043",
     Just (Forall [Star, Star, Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cRead (TGen 1),
              isIn1 cRead (TGen 2)] :=>
             (tInt `fn` tReadS (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))))),
     [([PVar "v204", PVar "v205"],
       ap [elambda ([PVar "v206"],
                    eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "v209"]) (ap [evar "lex", evar "v206"])
                    (eCompFrom (PCon tup2Cfun [PVar "v208", PVar "v210"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v209"])
                     (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v212"]) (ap [evar "lex", evar "v210"])
                      (eCompFrom (PCon tup2Cfun [PVar "v211", PVar "v213"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v212"])
                       (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v215"]) (ap [evar "lex", evar "v213"])
                        (eCompFrom (PCon tup2Cfun [PVar "v214", PVar "v216"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v215"])
                         (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "v207"]) (ap [evar "lex", evar "v216"])
                          (eListRet (ap [econst tup2Cfun, ap [econst tup3Cfun, evar "v208", evar "v211", evar "v214"], evar "v207"]))))))))), evar "v205"])]),
    ("v1047",
     Just (Forall [Star, Star, Star]
            ([isIn1 cShow (TGen 0),
              isIn1 cShow (TGen 1),
              isIn1 cShow (TGen 2)] :=>
             (tInt `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` tShowS))),
     [([PVar "v157", PCon tup3Cfun [PVar "v156", PVar "v155", PVar "v154"]],
       ap [evar ".", ap [econst consCfun, elit (LitChar '(')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v156"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v155"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v154"], ap [econst consCfun, elit (LitChar ')')]]]]]]])]),
    ("v1051",
     Just (Forall [Star, Star, Star]
            ([isIn1 cOrd (TGen 0),
              isIn1 cOrd (TGen 1),
              isIn1 cOrd (TGen 2)] :=>
             (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` tOrdering))),
     [([PCon tup3Cfun [PVar "v154", PVar "v153", PVar "v152"], PCon tup3Cfun [PVar "v157", PVar "v156", PVar "v155"]],
       ap [evar "primCompAux", evar "v154", evar "v157", ap [evar "primCompAux", evar "v153", evar "v156", ap [econst compareMfun, evar "v152", evar "v155"]]])]),
    ("v1059",
     Just (Forall [Star, Star, Star]
            ([isIn1 cEq (TGen 0),
              isIn1 cEq (TGen 1),
              isIn1 cEq (TGen 2)] :=>
             (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2) `fn` tBool))),
     [([PCon tup3Cfun [PVar "v154", PVar "v153", PVar "v152"], PCon tup3Cfun [PVar "v157", PVar "v156", PVar "v155"]],
       ap [evar "&&", ap [econst eqMfun, evar "v154", evar "v157"], ap [evar "&&", ap [econst eqMfun, evar "v153", evar "v156"], ap [econst eqMfun, evar "v152", evar "v155"]]])]),
    ("v1063",
     Just (Forall [Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1)] :=>
             (TAp (TAp tTuple2 (TAp (TAp tTuple2 (TGen 0)) (TGen 1))) (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1))))),
     [([PCon tup2Cfun [PCon tup2Cfun [PVar "v157", PVar "v154"], PCon tup2Cfun [PVar "v156", PVar "v153"]]],
       eCompFrom (PVar "v155") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"]])
       (eCompFrom (PVar "v152") (ap [econst rangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]])
        (eListRet (ap [econst tup2Cfun, evar "v155", evar "v152"]))))]),
    ("v1064",
     Just (Forall [Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1)] :=>
             (TAp (TAp tTuple2 (TAp (TAp tTuple2 (TGen 0)) (TGen 1))) (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` tInt))),
     [([PCon tup2Cfun [PCon tup2Cfun [PVar "v157", PVar "v154"], PCon tup2Cfun [PVar "v156", PVar "v153"]], PCon tup2Cfun [PVar "v155", PVar "v152"]],
       ap [econst plusMfun, ap [econst indexMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"], ap [econst timesMfun, ap [econst rangeSizeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"]], ap [econst indexMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"]]])]),
    ("v1065",
     Just (Forall [Star, Star]
            ([isIn1 cIx (TGen 0),
              isIn1 cIx (TGen 1)] :=>
             (TAp (TAp tTuple2 (TAp (TAp tTuple2 (TGen 0)) (TGen 1))) (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` tBool))),
     [([PCon tup2Cfun [PCon tup2Cfun [PVar "v157", PVar "v154"], PCon tup2Cfun [PVar "v156", PVar "v153"]], PCon tup2Cfun [PVar "v155", PVar "v152"]],
       ap [evar "&&", ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v157", evar "v156"], evar "v155"], ap [econst inRangeMfun, ap [econst tup2Cfun, evar "v154", evar "v153"], evar "v152"]])]),
    ("v1068",
     Just (Forall [Star, Star]
            ([isIn1 cRead (TGen 0),
              isIn1 cRead (TGen 1)] :=>
             (tInt `fn` tReadS (TAp (TAp tTuple2 (TGen 0)) (TGen 1))))),
     [([PVar "v222", PVar "v223"],
       ap [elambda ([PVar "v224"],
                    eCompFrom (PCon tup2Cfun [PLit (LitStr "("), PVar "v227"]) (ap [evar "lex", evar "v224"])
                    (eCompFrom (PCon tup2Cfun [PVar "v226", PVar "v228"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v227"])
                     (eCompFrom (PCon tup2Cfun [PLit (LitStr ","), PVar "v230"]) (ap [evar "lex", evar "v228"])
                      (eCompFrom (PCon tup2Cfun [PVar "v229", PVar "v231"]) (ap [econst readsPrecMfun, elit (LitInt 0), evar "v230"])
                       (eCompFrom (PCon tup2Cfun [PLit (LitStr ")"), PVar "v225"]) (ap [evar "lex", evar "v231"])
                        (eListRet (ap [econst tup2Cfun, ap [econst tup2Cfun, evar "v226", evar "v229"], evar "v225"]))))))), evar "v223"])]),
    ("v1072",
     Just (Forall [Star, Star]
            ([isIn1 cShow (TGen 0),
              isIn1 cShow (TGen 1)] :=>
             (tInt `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` tShowS))),
     [([PVar "v157", PCon tup2Cfun [PVar "v156", PVar "v155"]],
       ap [evar ".", ap [econst consCfun, elit (LitChar '(')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v156"], ap [evar ".", ap [econst consCfun, elit (LitChar ',')], ap [evar ".", ap [econst showsPrecMfun, elit (LitInt 0), evar "v155"], ap [econst consCfun, elit (LitChar ')')]]]]])]),
    ("v1076",
     Just (Forall [Star, Star]
            ([isIn1 cOrd (TGen 0),
              isIn1 cOrd (TGen 1)] :=>
             (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` tOrdering))),
     [([PCon tup2Cfun [PVar "v155", PVar "v154"], PCon tup2Cfun [PVar "v157", PVar "v156"]],
       ap [evar "primCompAux", evar "v155", evar "v157", ap [econst compareMfun, evar "v154", evar "v156"]])]),
    ("v1084",
     Just (Forall [Star, Star]
            ([isIn1 cEq (TGen 0),
              isIn1 cEq (TGen 1)] :=>
             (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` tBool))),
     [([PCon tup2Cfun [PVar "v155", PVar "v154"], PCon tup2Cfun [PVar "v157", PVar "v156"]],
       ap [evar "&&", ap [econst eqMfun, evar "v155", evar "v157"], ap [econst eqMfun, evar "v154", evar "v156"]])]) ]

-----------------------------------------------------------------------------
