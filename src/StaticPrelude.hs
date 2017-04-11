-----------------------------------------------------------------------------
-- StaticPrelude:	Types and Classes in the Standard Haskell Prelude
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

module StaticPrelude where
import Static

-----------------------------------------------------------------------------
-- Standard Primitive Types:

-----------------------------------------------------------------------------
-- Definitions for the following primitive types are in Type.hs:
--   (), Char, Int, Integer, Float, Double, [], (->), and tuples size 2 to 7
--
-- Type assumptions for the constructors of these types are provided below:

unitCfun = "()" :>: (Forall [] ([] :=> tUnit))

nilCfun  = "[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))
consCfun = ":"  :>: (Forall [Star]
                     ([] :=> 
                      (TGen 0 `fn`
                       TAp tList (TGen 0) `fn`
                       TAp tList (TGen 0))))

tup2Cfun = "(,)" :>: (Forall [Star, Star]
                      ([] :=> 
                       (TGen 0 `fn`
                        TGen 1 `fn`
                        foldl TAp tTuple2 (map TGen [0..1]))))

tup3Cfun = "(,,)" :>: (Forall [Star, Star, Star]
                       ([] :=> 
                        (TGen 0 `fn`
                         TGen 1 `fn`
                         TGen 2 `fn`
                         foldl TAp tTuple3 (map TGen [0..2]))))

tup4Cfun = "(,,,)" :>: (Forall [Star, Star, Star, Star]
                        ([] :=> 
                         (TGen 0 `fn`
                          TGen 1 `fn`
                          TGen 2 `fn`
                          TGen 3 `fn`
                          foldl TAp tTuple4 (map TGen [0..3]))))

tup5Cfun = "(,,,,)" :>: (Forall [Star, Star, Star, Star, Star]
                         ([] :=> 
                          (TGen 0 `fn`
                           TGen 1 `fn`
                           TGen 2 `fn`
                           TGen 3 `fn`
                           TGen 4 `fn`
                           foldl TAp tTuple5 (map TGen [0..4]))))

tup6Cfun = "(,,,,,)" :>: (Forall [Star, Star, Star, Star, Star, Star]
                          ([] :=> 
                           (TGen 0 `fn`
                            TGen 1 `fn`
                            TGen 2 `fn`
                            TGen 3 `fn`
                            TGen 4 `fn`
                            TGen 5 `fn`
                            foldl TAp tTuple6 (map TGen [0..5]))))

tup7Cfun = "(,,,,,,)" :>: (Forall [Star, Star, Star, Star, Star, Star, Star]
                           ([] :=> 
                            (TGen 0 `fn`
                             TGen 1 `fn`
                             TGen 2 `fn`
                             TGen 3 `fn`
                             TGen 4 `fn`
                             TGen 5 `fn`
                             TGen 6 `fn`
                             foldl TAp tTuple7 (map TGen [0..6]))))

-----------------------------------------------------------------------------
-- The following types, and their associated constructors, are not defined
-- in the body of the paper

tReadS a
 = TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 a) (TAp tList tChar))
tShowS
 = TAp tList tChar `fn` TAp tList tChar

tBool     = TCon (Tycon "Bool" Star)
falseCfun = "False" :>: (Forall [] ([] :=> tBool))
trueCfun  = "True" :>: (Forall [] ([] :=> tBool))

tMaybe      = TCon (Tycon "Maybe" (Kfun Star Star))
nothingCfun = "Nothing" :>: (Forall [Star]
                             ([] :=> (TAp tMaybe (TGen 0))))
justCfun    = "Just" :>: (Forall [Star]
                          ([] :=> (TGen 0 `fn` TAp tMaybe (TGen 0))))

tEither   = TCon (Tycon "Either" (Kfun Star (Kfun Star Star)))
leftCfun  = "Left" :>: (Forall [Star, Star]
                        ([] :=> 
                         (TGen 0 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))
rightCfun = "Right" :>: (Forall [Star, Star]
                         ([] :=> 
                          (TGen 1 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))

tOrdering = TCon (Tycon "Ordering" Star)
lTCfun    = "LT" :>: (Forall [] ([] :=> tOrdering))
eQCfun    = "EQ" :>: (Forall [] ([] :=> tOrdering))
gTCfun    = "GT" :>: (Forall [] ([] :=> tOrdering))

tRatio    = TCon (Tycon "Ratio" (Kfun Star Star))
consratCfun = ":%" :>: (Forall [Star]
              ([isIn1 cIntegral (TGen 0)] :=> 
               (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0))))

tRational = TAp tRatio tInteger

tIOError  = TCon (Tycon "IOError" Star)

tFilePath = TAp tList tChar

tIO       = TCon (Tycon "IO" (Kfun Star Star))
iOCfun    = "IO" :>: (Forall [Star]
                      ([] :=> 
                       (((tIOError `fn` TAp tIOResult (TGen 0)) `fn`
                         (TGen 0 `fn` TAp tIOResult (TGen 0)) `fn`
                         TAp tIOResult (TGen 0)) `fn`
                          TAp tIO (TGen 0))))

tIOResult = TCon (Tycon "IOResult" (Kfun Star Star))
hugs_ExitWithCfun
          = "Hugs_ExitWith" :>: (Forall [Star]
                                 ([] :=> (tInt `fn` TAp tIOResult (TGen 0))))
hugs_SuspendThreadCfun
          = "Hugs_SuspendThread" :>: (Forall [Star]
                                      ([] :=> (TAp tIOResult (TGen 0))))
hugs_ErrorCfun
          = "Hugs_Error" :>: (Forall [Star]
                              ([] :=> (tIOError `fn` TAp tIOResult (TGen 0))))
hugs_ReturnCfun
          = "Hugs_Return" :>: (Forall [Star]
                               ([] :=> (TGen 0 `fn` TAp tIOResult (TGen 0))))

-----------------------------------------------------------------------------
-- Standard Classes and Member functions:
--
-- code to define these classes is provided in Preds.hs, so the code here
-- just defines the member functions and instances.

preludeClasses =   addPreludeClasses
               <:> addClass cIx asig [IsIn cOrd [atype]]
               <:> instances (instsEq ++ instsOrd ++ instsBounded ++
                              instsEnum ++ instsRead ++ instsShow ++
                              instsIx ++
                              instsFunctor ++ instsMonad ++
                              instsNum ++ instsReal ++ instsIntegral ++
                              instsFractional ++ instsFloating ++
                              instsRealFrac ++ instsRealFloat)

-- The Eq class -------------------------------------------------------------

cEq = "Eq"

eqMfun  = "==" :>: (Forall [Star]
                    ([isIn1 cEq (TGen 0)] :=> 
                     (TGen 0 `fn` TGen 0 `fn` tBool)))
neqMfun = "/=" :>: (Forall [Star]
                    ([isIn1 cEq (TGen 0)] :=> 
                     (TGen 0 `fn` TGen 0 `fn` tBool)))

instsEq
 = [mkInst [] ([] :=> isIn1 cEq tUnit),
    mkInst [] ([] :=> isIn1 cEq tChar),
    mkInst [Star]
     ([isIn1 cEq (TGen 0)] :=> 
      isIn1 cEq (TAp tList (TGen 0))),
    mkInst [] ([] :=> isIn1 cEq tInt),
    mkInst [] ([] :=> isIn1 cEq tInteger),
    mkInst [] ([] :=> isIn1 cEq tFloat),
    mkInst [] ([] :=> isIn1 cEq tDouble),
    mkInst [] ([] :=> isIn1 cEq tBool),
    mkInst [Star]
     ([isIn1 cEq (TGen 0)] :=> 
      isIn1 cEq (TAp tMaybe (TGen 0))),
    mkInst [Star, Star]
     ([isIn1 cEq (TGen 0),
       isIn1 cEq (TGen 1)] :=> 
      isIn1 cEq (TAp (TAp tEither (TGen 0)) (TGen 1))),
    mkInst [] ([] :=> isIn1 cEq tOrdering),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cEq (TAp tRatio (TGen 0))),
    mkInst [Star, Star, Star, Star, Star]
     ([isIn1 cEq (TGen 0),
       isIn1 cEq (TGen 1),
       isIn1 cEq (TGen 2),
       isIn1 cEq (TGen 3),
       isIn1 cEq (TGen 4)] :=> 
      isIn1 cEq (foldl TAp tTuple5 (map TGen [0..4]))),
    mkInst [Star, Star, Star, Star]
     ([isIn1 cEq (TGen 0),
       isIn1 cEq (TGen 1),
       isIn1 cEq (TGen 2),
       isIn1 cEq (TGen 3)] :=> 
      isIn1 cEq (foldl TAp tTuple4 (map TGen [0..3]))),
    mkInst [Star, Star, Star]
     ([isIn1 cEq (TGen 0),
       isIn1 cEq (TGen 1),
       isIn1 cEq (TGen 2)] :=> 
      isIn1 cEq (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))),
    mkInst [Star, Star]
     ([isIn1 cEq (TGen 0),
       isIn1 cEq (TGen 1)] :=> 
      isIn1 cEq (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))]

-- The Ord class ------------------------------------------------------------

cOrd = "Ord"

compareMfun
 = "compare" :>: (Forall [Star]
                   ([isIn1 cOrd (TGen 0)] :=> 
                    (TGen 0 `fn` TGen 0 `fn` tOrdering)))
ltMfun
 = "<" :>: (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` tBool)))
leMfun
 = "<=" :>: (Forall [Star]
              ([isIn1 cOrd (TGen 0)] :=> 
               (TGen 0 `fn` TGen 0 `fn` tBool)))
geMfun
 = ">=" :>: (Forall [Star]
              ([isIn1 cOrd (TGen 0)] :=> 
               (TGen 0 `fn` TGen 0 `fn` tBool)))
gtMfun
 = ">" :>: (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` tBool)))
maxMfun
 = "max" :>: (Forall [Star]
               ([isIn1 cOrd (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minMfun
 = "min" :>: (Forall [Star]
               ([isIn1 cOrd (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))

instsOrd
 = [mkInst [] ([] :=> isIn1 cOrd tUnit),
    mkInst [] ([] :=> isIn1 cOrd tChar),
    mkInst [Star]
     ([isIn1 cOrd (TGen 0)] :=> 
      isIn1 cOrd (TAp tList (TGen 0))),
    mkInst [] ([] :=> isIn1 cOrd tInt),
    mkInst [] ([] :=> isIn1 cOrd tInteger),
    mkInst [] ([] :=> isIn1 cOrd tFloat),
    mkInst [] ([] :=> isIn1 cOrd tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cOrd (TAp tRatio (TGen 0))),
    mkInst [] ([] :=> isIn1 cOrd tBool),
    mkInst [Star]
     ([isIn1 cOrd (TGen 0)] :=> 
      isIn1 cOrd (TAp tMaybe (TGen 0))),
    mkInst [Star, Star]
     ([isIn1 cOrd (TGen 0),
       isIn1 cOrd (TGen 1)] :=> 
      isIn1 cOrd (TAp (TAp tEither (TGen 0)) (TGen 1))),
    mkInst [] ([] :=> isIn1 cOrd tOrdering),
    mkInst [Star, Star, Star, Star, Star]
     ([isIn1 cOrd (TGen 0),
       isIn1 cOrd (TGen 1),
       isIn1 cOrd (TGen 2),
       isIn1 cOrd (TGen 3),
       isIn1 cOrd (TGen 4)] :=> 
      isIn1 cOrd (foldl TAp tTuple5 (map TGen [0..4]))),
    mkInst [Star, Star, Star, Star]
     ([isIn1 cOrd (TGen 0),
       isIn1 cOrd (TGen 1),
       isIn1 cOrd (TGen 2),
       isIn1 cOrd (TGen 3)] :=> 
      isIn1 cOrd (foldl TAp tTuple4 (map TGen [0..3]))),
    mkInst [Star, Star, Star]
     ([isIn1 cOrd (TGen 0),
       isIn1 cOrd (TGen 1),
       isIn1 cOrd (TGen 2)] :=> 
      isIn1 cOrd (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))),
    mkInst [Star, Star]
     ([isIn1 cOrd (TGen 0),
       isIn1 cOrd (TGen 1)] :=> 
      isIn1 cOrd (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))]

-- The Bounded class --------------------------------------------------------

cBounded = "Bounded"

minBoundMfun
 = "minBound" :>: (Forall [Star]
                    ([isIn1 cBounded (TGen 0)] :=> 
                     (TGen 0)))
maxBoundMfun
 = "maxBound" :>: (Forall [Star]
                    ([isIn1 cBounded (TGen 0)] :=> 
                     (TGen 0)))

instsBounded
 = [mkInst [] ([] :=> isIn1 cBounded tUnit),
    mkInst [] ([] :=> isIn1 cBounded tChar),
    mkInst [] ([] :=> isIn1 cBounded tInt),
    mkInst [] ([] :=> isIn1 cBounded tBool),
    mkInst [] ([] :=> isIn1 cBounded tOrdering)]

-- The Num class ------------------------------------------------------------

cNum = "Num"

plusMfun
 = "+" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minusMfun
 = "-" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
timesMfun
 = "*" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
negateMfun
 = "negate" :>: (Forall [Star]
                  ([isIn1 cNum (TGen 0)] :=> 
                   (TGen 0 `fn` TGen 0)))
absMfun
 = "abs" :>: (Forall [Star]
               ([isIn1 cNum (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0)))
signumMfun
 = "signum" :>: (Forall [Star]
                  ([isIn1 cNum (TGen 0)] :=> 
                   (TGen 0 `fn` TGen 0)))
fromIntegerMfun
 = "fromInteger" :>: (Forall [Star]
                       ([isIn1 cNum (TGen 0)] :=> 
                        (tInteger `fn` TGen 0)))
fromIntMfun
 = "fromInt" :>: (Forall [Star]
                   ([isIn1 cNum (TGen 0)] :=> 
                    (tInt `fn` TGen 0)))

instsNum
 = [mkInst [] ([] :=> isIn1 cNum tInt),
    mkInst [] ([] :=> isIn1 cNum tInteger),
    mkInst [] ([] :=> isIn1 cNum tFloat),
    mkInst [] ([] :=> isIn1 cNum tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cNum (TAp tRatio (TGen 0)))]

-- The Real class -----------------------------------------------------------

cReal = "Real"

toRationalMfun
 = "toRational" :>: (Forall [Star]
                      ([isIn1 cReal (TGen 0)] :=> 
                       (TGen 0 `fn` tRational)))

instsReal
 = [mkInst [] ([] :=> isIn1 cReal tInt),
    mkInst [] ([] :=> isIn1 cReal tInteger),
    mkInst [] ([] :=> isIn1 cReal tFloat),
    mkInst [] ([] :=> isIn1 cReal tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cReal (TAp tRatio (TGen 0)))]

-- The Integral class -------------------------------------------------------

cIntegral = "Integral"

quotMfun
 = "quot" :>: (Forall [Star]
                ([isIn1 cIntegral (TGen 0)] :=> 
                 (TGen 0 `fn` TGen 0 `fn` TGen 0)))
remMfun
 = "rem" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
divMfun
 = "div" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
modMfun
 = "mod" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=> 
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
quotRemMfun
 = "quotRem" :>: (Forall [Star]
                   ([isIn1 cIntegral (TGen 0)] :=> 
                    (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
divModMfun
 = "divMod" :>: (Forall [Star]
                  ([isIn1 cIntegral (TGen 0)] :=> 
                   (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
evenMfun
 = "even" :>: (Forall [Star]
                ([isIn1 cIntegral (TGen 0)] :=> 
                 (TGen 0 `fn` tBool)))
oddMfun
 = "odd" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=> 
                (TGen 0 `fn` tBool)))
toIntegerMfun
 = "toInteger" :>: (Forall [Star]
                     ([isIn1 cIntegral (TGen 0)] :=> 
                      (TGen 0 `fn` tInteger)))
toIntMfun
 = "toInt" :>: (Forall [Star]
                 ([isIn1 cIntegral (TGen 0)] :=> 
                  (TGen 0 `fn` tInt)))

instsIntegral
 = [mkInst [] ([] :=> isIn1 cIntegral tInt),
    mkInst [] ([] :=> isIn1 cIntegral tInteger)]

-- The Fractional class -----------------------------------------------------

cFractional = "Fractional"

divideMfun
 = "/" :>: (Forall [Star]
             ([isIn1 cFractional (TGen 0)] :=> 
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
recipMfun
 = "recip" :>: (Forall [Star]
                 ([isIn1 cFractional (TGen 0)] :=> 
                  (TGen 0 `fn` TGen 0)))
fromRationalMfun
 = "fromRational" :>: (Forall [Star]
                        ([isIn1 cFractional (TGen 0)] :=> 
                         (tRational `fn` TGen 0)))
fromDoubleMfun
 = "fromDouble" :>: (Forall [Star]
                      ([isIn1 cFractional (TGen 0)] :=> 
                       (tDouble `fn` TGen 0)))

instsFractional
 = [mkInst [] ([] :=> isIn1 cFractional tFloat),
    mkInst [] ([] :=> isIn1 cFractional tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cFractional (TAp tRatio (TGen 0)))]

-- The Floating class -------------------------------------------------------

cFloating = "Floating"

piMfun
 = "pi" :>: (Forall [Star]
              ([isIn1 cFloating (TGen 0)] :=> (TGen 0)))
expMfun
 = "exp" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
logMfun
 = "log" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sqrtMfun
 = "sqrt" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
starstarMfun
 = "**" :>: (Forall [Star]
              ([isIn1 cFloating (TGen 0)] :=> 
               (TGen 0 `fn` TGen 0 `fn` TGen 0)))
logBaseMfun
 = "logBase" :>: (Forall [Star]
                   ([isIn1 cFloating (TGen 0)] :=> 
                    (TGen 0 `fn` TGen 0 `fn` TGen 0)))
sinMfun
 = "sin" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
cosMfun
 = "cos" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanMfun
 = "tan" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinMfun
 = "asin" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acosMfun
 = "acos" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanMfun
 = "atan" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sinhMfun
 = "sinh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
coshMfun
 = "cosh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanhMfun
 = "tanh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinhMfun
 = "asinh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acoshMfun
 = "acosh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanhMfun
 = "atanh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))

instsFloating
 = [mkInst [] ([] :=> isIn1 cFloating tFloat),
    mkInst [] ([] :=> isIn1 cFloating tDouble)]

-- The RealFrac class -------------------------------------------------------

cRealFrac = "RealFrac"

properFractionMfun
 = "properFraction" :>: (Forall [Star, Star]
                          ([isIn1 cRealFrac (TGen 0),
                            isIn1 cIntegral (TGen 1)] :=> 
                           (TGen 0 `fn` TAp (TAp tTuple2 (TGen 1)) (TGen 0))))
truncateMfun
 = "truncate" :>: (Forall [Star, Star]
                    ([isIn1 cRealFrac (TGen 0),
                      isIn1 cIntegral (TGen 1)] :=> 
                     (TGen 0 `fn` TGen 1)))
roundMfun
 = "round" :>: (Forall [Star, Star]
                 ([isIn1 cRealFrac (TGen 0),
                   isIn1 cIntegral (TGen 1)] :=> 
                  (TGen 0 `fn` TGen 1)))
ceilingMfun
 = "ceiling" :>: (Forall [Star, Star]
                   ([isIn1 cRealFrac (TGen 0),
                     isIn1 cIntegral (TGen 1)] :=> 
                    (TGen 0 `fn` TGen 1)))
floorMfun
 = "floor" :>: (Forall [Star, Star]
                 ([isIn1 cRealFrac (TGen 0),
                   isIn1 cIntegral (TGen 1)] :=> 
                  (TGen 0 `fn` TGen 1)))

instsRealFrac
 = [mkInst [] ([] :=> isIn1 cRealFrac tFloat),
    mkInst [] ([] :=> isIn1 cRealFrac tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> isIn1 cRealFrac (TAp tRatio (TGen 0)))]

-- The RealFloat class ------------------------------------------------------

cRealFloat = "RealFloat"

floatRadixMfun
 = "floatRadix" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=> 
                       (TGen 0 `fn` tInteger)))
floatDigitsMfun
 = "floatDigits" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=> 
                        (TGen 0 `fn` tInt)))
floatRangeMfun
 = "floatRange" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=> 
                       (TGen 0 `fn` TAp (TAp tTuple2 tInt) tInt)))
decodeFloatMfun
 = "decodeFloat" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=> 
                        (TGen 0 `fn` TAp (TAp tTuple2 tInteger) tInt)))
encodeFloatMfun
 = "encodeFloat" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=> 
                        (tInteger `fn` tInt `fn` TGen 0)))
exponentMfun
 = "exponent" :>: (Forall [Star]
                    ([isIn1 cRealFloat (TGen 0)] :=> 
                     (TGen 0 `fn` tInt)))
significandMfun
 = "significand" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=> 
                        (TGen 0 `fn` TGen 0)))
scaleFloatMfun
 = "scaleFloat" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=> 
                       (tInt `fn` TGen 0 `fn` TGen 0)))
isNaNMfun
 = "isNaN" :>: (Forall [Star]
                 ([isIn1 cRealFloat (TGen 0)] :=> 
                  (TGen 0 `fn` tBool)))
isInfiniteMfun
 = "isInfinite" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=> 
                       (TGen 0 `fn` tBool)))
isDenormalizedMfun
 = "isDenormalized" :>: (Forall [Star]
                          ([isIn1 cRealFloat (TGen 0)] :=> 
                           (TGen 0 `fn` tBool)))
isNegativeZeroMfun
 = "isNegativeZero" :>: (Forall [Star]
                          ([isIn1 cRealFloat (TGen 0)] :=> 
                           (TGen 0 `fn` tBool)))
isIEEEMfun
 = "isIEEE" :>: (Forall [Star]
                  ([isIn1 cRealFloat (TGen 0)] :=> 
                   (TGen 0 `fn` tBool)))
atan2Mfun
 = "atan2" :>: (Forall [Star]
                 ([isIn1 cRealFloat (TGen 0)] :=> 
                  (TGen 0 `fn` TGen 0 `fn` TGen 0)))

instsRealFloat
 = [mkInst [] ([] :=> isIn1 cRealFloat tFloat),
    mkInst [] ([] :=> isIn1 cRealFloat tDouble)]

-- The Enum class -----------------------------------------------------------

cEnum = "Enum"

succMfun
 = "succ" :>: (Forall [Star]
                ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
predMfun
 = "pred" :>: (Forall [Star]
                ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
toEnumMfun
 = "toEnum" :>: (Forall [Star]
                  ([isIn1 cEnum (TGen 0)] :=> (tInt `fn` TGen 0)))
fromEnumMfun
 = "fromEnum" :>: (Forall [Star]
                    ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` tInt)))
enumFromMfun
 = "enumFrom" :>: (Forall [Star]
                    ([isIn1 cEnum (TGen 0)] :=> 
                     (TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenMfun
 = "enumFromThen" :>: (Forall [Star]
                        ([isIn1 cEnum (TGen 0)] :=> 
                         (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromToMfun
 = "enumFromTo" :>: (Forall [Star]
                      ([isIn1 cEnum (TGen 0)] :=> 
                       (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenToMfun
 = "enumFromThenTo" :>: (Forall [Star]
                          ([isIn1 cEnum (TGen 0)] :=> 
                           (TGen 0 `fn` TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))

instsEnum
 = [mkInst [] ([] :=> isIn1 cEnum tUnit),
    mkInst [] ([] :=> isIn1 cEnum tChar),
    mkInst [] ([] :=> isIn1 cEnum tInt),
    mkInst [] ([] :=> isIn1 cEnum tInteger),
    mkInst [] ([] :=> isIn1 cEnum tFloat),
    mkInst [] ([] :=> isIn1 cEnum tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cEnum (TAp tRatio (TGen 0))),
    mkInst [] ([] :=> isIn1 cEnum tBool),
    mkInst [] ([] :=> isIn1 cEnum tOrdering)]

-- The Read class -----------------------------------------------------------

cRead = "Read"

readsPrecMfun
 = "readsPrec" :>: (Forall [Star]
                     ([isIn1 cRead (TGen 0)] :=> (tInt `fn` tReadS (TGen 0))))
readListMfun
 = "readList" :>: (Forall [Star]
                    ([isIn1 cRead (TGen 0)] :=> tReadS (TAp tList (TGen 0))))

instsRead
 = [mkInst [] ([] :=> isIn1 cRead tUnit),
    mkInst [] ([] :=> isIn1 cRead tChar),
    mkInst [Star]
     ([isIn1 cRead (TGen 0)] :=> isIn1 cRead (TAp tList (TGen 0))),
    mkInst [] ([] :=> isIn1 cRead tInt),
    mkInst [] ([] :=> isIn1 cRead tInteger),
    mkInst [] ([] :=> isIn1 cRead tFloat),
    mkInst [] ([] :=> isIn1 cRead tDouble),
    mkInst [Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cIntegral (TGen 0)] :=> isIn1 cRead (TAp tRatio (TGen 0))),
    mkInst [] ([] :=> isIn1 cRead tBool),
    mkInst [Star]
     ([isIn1 cRead (TGen 0)] :=> isIn1 cRead (TAp tMaybe (TGen 0))),
    mkInst [Star, Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cRead (TGen 1)] :=> 
      isIn1 cRead (TAp (TAp tEither (TGen 0)) (TGen 1))),
    mkInst [] ([] :=> isIn1 cRead tOrdering),
    mkInst [Star, Star, Star, Star, Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cRead (TGen 1),
       isIn1 cRead (TGen 2),
       isIn1 cRead (TGen 3),
       isIn1 cRead (TGen 4)] :=> 
      isIn1 cRead (foldl TAp tTuple5 (map TGen [0..4]))),
    mkInst [Star, Star, Star, Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cRead (TGen 1),
       isIn1 cRead (TGen 2),
       isIn1 cRead (TGen 3)] :=> 
      isIn1 cRead (foldl TAp tTuple4 (map TGen [0..3]))),
    mkInst [Star, Star, Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cRead (TGen 1),
       isIn1 cRead (TGen 2)] :=> 
      isIn1 cRead (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))),
    mkInst [Star, Star]
     ([isIn1 cRead (TGen 0),
       isIn1 cRead (TGen 1)] :=> 
      isIn1 cRead (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))]

-- The Show class -----------------------------------------------------------

cShow = "Show"

showMfun
 = "show" :>: (Forall [Star]
                ([isIn1 cShow (TGen 0)] :=> 
                 (TGen 0 `fn` tString)))
showsPrecMfun
 = "showsPrec" :>: (Forall [Star]
                     ([isIn1 cShow (TGen 0)] :=> 
                      (tInt `fn` TGen 0 `fn` tShowS)))
showListMfun
 = "showList" :>: (Forall [Star]
                    ([isIn1 cShow (TGen 0)] :=> 
                     (TAp tList (TGen 0) `fn` tShowS)))

instsShow
 = [mkInst [] ([] :=> isIn1 cShow tUnit),
    mkInst [] ([] :=> isIn1 cShow tChar),
    mkInst [Star]
     ([isIn1 cShow (TGen 0)] :=> 
      isIn1 cShow (TAp tList (TGen 0))),
    mkInst [] ([] :=> isIn1 cShow tInt),
    mkInst [] ([] :=> isIn1 cShow tInteger),
    mkInst [] ([] :=> isIn1 cShow tFloat),
    mkInst [] ([] :=> isIn1 cShow tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> 
      isIn1 cShow (TAp tRatio (TGen 0))),
    mkInst [Star] ([] :=> isIn1 cShow (TAp tIO (TGen 0))),
    mkInst [] ([] :=> isIn1 cShow tIOError),
    mkInst [] ([] :=> isIn1 cShow tBool),
    mkInst [Star]
     ([isIn1 cShow (TGen 0)] :=> 
      isIn1 cShow (TAp tMaybe (TGen 0))),
    mkInst [Star, Star]
     ([isIn1 cShow (TGen 0),
       isIn1 cShow (TGen 1)] :=> 
      isIn1 cShow (TAp (TAp tEither (TGen 0)) (TGen 1))),
    mkInst [] ([] :=> isIn1 cShow tOrdering),
    mkInst [Star, Star, Star, Star, Star]
     ([isIn1 cShow (TGen 0),
       isIn1 cShow (TGen 1),
       isIn1 cShow (TGen 2),
       isIn1 cShow (TGen 3),
       isIn1 cShow (TGen 4)] :=> 
      isIn1 cShow (foldl TAp tTuple5 (map TGen [0..4]))),
    mkInst [Star, Star, Star, Star]
     ([isIn1 cShow (TGen 0),
       isIn1 cShow (TGen 1),
       isIn1 cShow (TGen 2),
       isIn1 cShow (TGen 3)] :=> 
      isIn1 cShow (foldl TAp tTuple4 (map TGen [0..3]))),
    mkInst [Star, Star, Star]
     ([isIn1 cShow (TGen 0),
       isIn1 cShow (TGen 1),
       isIn1 cShow (TGen 2)] :=> 
      isIn1 cShow (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))),
    mkInst [Star, Star]
     ([isIn1 cShow (TGen 0),
       isIn1 cShow (TGen 1)] :=> 
      isIn1 cShow (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))]

-- The Ix class -------------------------------------------------------------

cIx = "Ix"

rangeMfun
 = "range" :>: (Forall [Star]
                ([isIn1 cIx (TGen 0)] :=> 
                 (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                  TAp tList (TGen 0))))
indexMfun
 = "index" :>: (Forall [Star]
                ([isIn1 cIx (TGen 0)] :=> 
                 (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                  TGen 0 `fn` tInt)))
inRangeMfun
 = "inRange" :>: (Forall [Star]
                  ([isIn1 cIx (TGen 0)] :=> 
                   (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                    TGen 0 `fn` tBool)))
rangeSizeMfun
 = "rangeSize" :>: (Forall [Star]
                    ([isIn1 cIx (TGen 0)] :=> 
                     (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` tInt)))

instsIx
 = [mkInst [] ([] :=> isIn1 cIx tUnit),
    mkInst [] ([] :=> isIn1 cIx tChar),
    mkInst [] ([] :=> isIn1 cIx tInt),
    mkInst [] ([] :=> isIn1 cIx tInteger),
    mkInst [] ([] :=> isIn1 cIx tBool),
    mkInst [] ([] :=> isIn1 cIx tOrdering),
    mkInst [Star, Star, Star, Star, Star]
     ([isIn1 cIx (TGen 0),
       isIn1 cIx (TGen 1),
       isIn1 cIx (TGen 2),
       isIn1 cIx (TGen 3),
       isIn1 cIx (TGen 4)] :=> 
      isIn1 cIx (foldl TAp tTuple5 (map TGen [0..4]))),
    mkInst [Star, Star, Star, Star]
     ([isIn1 cIx (TGen 0),
       isIn1 cIx (TGen 1),
       isIn1 cIx (TGen 2),
       isIn1 cIx (TGen 3)] :=> 
      isIn1 cIx (foldl TAp tTuple4 (map TGen [0..3]))),
    mkInst [Star, Star, Star]
     ([isIn1 cIx (TGen 0),
       isIn1 cIx (TGen 1),
       isIn1 cIx (TGen 2)] :=> 
      isIn1 cIx (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2))),
    mkInst [Star, Star]
     ([isIn1 cIx (TGen 0),
       isIn1 cIx (TGen 1)] :=> 
      isIn1 cIx (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))]

-- The Functor class --------------------------------------------------------

cFunctor = "Functor"

fmapMfun
 = "fmap" :>: (Forall [Kfun Star Star, Star, Star]
                ([isIn1 cFunctor (TGen 0)] :=> 
                 ((TGen 1 `fn` TGen 2) `fn`
                  TAp (TGen 0) (TGen 1) `fn`
                  TAp (TGen 0) (TGen 2))))

instsFunctor
 = [mkInst [] ([] :=> isIn1 cFunctor tMaybe),
    mkInst [] ([] :=> isIn1 cFunctor tList),
    mkInst [] ([] :=> isIn1 cFunctor tIO)]

-- The Monad class ----------------------------------------------------------

cMonad = "Monad"

returnMfun
 = "return" :>: (Forall [Kfun Star Star, Star]
                  ([isIn1 cMonad (TGen 0)] :=> 
                   (TGen 1 `fn` TAp (TGen 0) (TGen 1))))
mbindMfun
 = ">>=" :>: (Forall [Kfun Star Star, Star, Star]
               ([isIn1 cMonad (TGen 0)] :=> 
                (TAp (TGen 0) (TGen 1) `fn` (TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 2))))
mthenMfun
 = ">>" :>: (Forall [Kfun Star Star, Star, Star]
              ([isIn1 cMonad (TGen 0)] :=> 
               (TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 2))))
failMfun
 = "fail" :>: (Forall [Kfun Star Star, Star]
                ([isIn1 cMonad (TGen 0)] :=> 
                 (tString `fn` TAp (TGen 0) (TGen 1))))

instsMonad
 = [mkInst [] ([] :=> isIn1 cMonad tMaybe),
    mkInst [] ([] :=> isIn1 cMonad tList),
    mkInst [] ([] :=> isIn1 cMonad tIO)]

-----------------------------------------------------------------------------
