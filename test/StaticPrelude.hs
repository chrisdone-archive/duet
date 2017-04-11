-----------------------------------------------------------------------------
-- StaticPrelude:       Types and Classes in the Standard Haskell Prelude
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
import TIMain
import Pat
pNil :: Pat
pNil            = PCon nilCfun []
pCons :: Pat -> Pat -> Pat
pCons x y       = PCon consCfun [x,y]

eNil :: Expr
eNil            = econst nilCfun
eCons :: Expr -> Expr -> Expr
eCons x y       = ap [ econst consCfun, x, y ]

eif :: Expr -> Expr -> Expr -> Expr
eif c t f       = ecase c [(PCon trueCfun [], t),(PCon falseCfun [], f)]

eguarded :: Foldable t => t (Expr, Expr) -> Expr
eguarded        = foldr (\(c,t) e -> eif c t e) efail

eCompFrom :: Pat -> Expr -> Expr -> Expr
eCompFrom p e c = ap [ econst mbindMfun, e, elambda ([p],c) ]

eCompGuard :: Expr -> Expr -> Expr
eCompGuard e c  = eif e c eNil

eListRet :: Expr -> Expr
eListRet e      = eCons e eNil

-----------------------------------------------------------------------------
-- Standard Primitive Types:

-----------------------------------------------------------------------------
-- Definitions for the following primitive types are in Type.hs:
--   (), Char, Int, Integer, Float, Double, [], (->), and tuples size 2 to 7
--
-- Type assumptions for the constructors of these types are provided below:

unitCfun :: Assump
unitCfun = "()" :>: (Forall [] ([] :=> tUnit))

nilCfun :: Assump
nilCfun  = "[]" :>: (Forall [Star] ([] :=> (TAp tList (TGen 0))))
consCfun :: Assump
consCfun = ":"  :>: (Forall [Star]
                     ([] :=>
                      (TGen 0 `fn`
                       TAp tList (TGen 0) `fn`
                       TAp tList (TGen 0))))

tup2Cfun :: Assump
tup2Cfun = "(,)" :>: (Forall [Star, Star]
                      ([] :=>
                       (TGen 0 `fn`
                        TGen 1 `fn`
                        foldl TAp tTuple2 (map TGen [0..1]))))

tup3Cfun :: Assump
tup3Cfun = "(,,)" :>: (Forall [Star, Star, Star]
                       ([] :=>
                        (TGen 0 `fn`
                         TGen 1 `fn`
                         TGen 2 `fn`
                         foldl TAp tTuple3 (map TGen [0..2]))))

tup4Cfun :: Assump
tup4Cfun = "(,,,)" :>: (Forall [Star, Star, Star, Star]
                        ([] :=>
                         (TGen 0 `fn`
                          TGen 1 `fn`
                          TGen 2 `fn`
                          TGen 3 `fn`
                          foldl TAp tTuple4 (map TGen [0..3]))))

tup5Cfun :: Assump
tup5Cfun = "(,,,,)" :>: (Forall [Star, Star, Star, Star, Star]
                         ([] :=>
                          (TGen 0 `fn`
                           TGen 1 `fn`
                           TGen 2 `fn`
                           TGen 3 `fn`
                           TGen 4 `fn`
                           foldl TAp tTuple5 (map TGen [0..4]))))

tup6Cfun :: Assump
tup6Cfun = "(,,,,,)" :>: (Forall [Star, Star, Star, Star, Star, Star]
                          ([] :=>
                           (TGen 0 `fn`
                            TGen 1 `fn`
                            TGen 2 `fn`
                            TGen 3 `fn`
                            TGen 4 `fn`
                            TGen 5 `fn`
                            foldl TAp tTuple6 (map TGen [0..5]))))

tup7Cfun :: Assump
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

tReadS :: Type -> Type
tReadS a
 = TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 a) (TAp tList tChar))
tShowS :: Type
tShowS
 = TAp tList tChar `fn` TAp tList tChar


falseCfun :: Assump
falseCfun = "False" :>: (Forall [] ([] :=> tBool))
trueCfun :: Assump
trueCfun  = "True" :>: (Forall [] ([] :=> tBool))

tMaybe :: Type
tMaybe      = TCon (Tycon "Maybe" (Kfun Star Star))
nothingCfun :: Assump
nothingCfun = "Nothing" :>: (Forall [Star]
                             ([] :=> (TAp tMaybe (TGen 0))))
justCfun :: Assump
justCfun    = "Just" :>: (Forall [Star]
                          ([] :=> (TGen 0 `fn` TAp tMaybe (TGen 0))))

tEither :: Type
tEither   = TCon (Tycon "Either" (Kfun Star (Kfun Star Star)))
leftCfun :: Assump
leftCfun  = "Left" :>: (Forall [Star, Star]
                        ([] :=>
                         (TGen 0 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))
rightCfun :: Assump
rightCfun = "Right" :>: (Forall [Star, Star]
                         ([] :=>
                          (TGen 1 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))

tOrdering :: Type
tOrdering = TCon (Tycon "Ordering" Star)
lTCfun :: Assump
lTCfun    = "LT" :>: (Forall [] ([] :=> tOrdering))
eQCfun :: Assump
eQCfun    = "EQ" :>: (Forall [] ([] :=> tOrdering))
gTCfun :: Assump
gTCfun    = "GT" :>: (Forall [] ([] :=> tOrdering))

tRatio :: Type
tRatio    = TCon (Tycon "Ratio" (Kfun Star Star))
consratCfun :: Assump
consratCfun = ":%" :>: (Forall [Star]
              ([isIn1 cIntegral (TGen 0)] :=>
               (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0))))

tRational :: Type
tRational = TAp tRatio tInteger

tIOError :: Type
tIOError  = TCon (Tycon "IOError" Star)

tFilePath :: Type
tFilePath = TAp tList tChar

tIO :: Type
tIO       = TCon (Tycon "IO" (Kfun Star Star))
iOCfun :: Assump
iOCfun    = "IO" :>: (Forall [Star]
                      ([] :=>
                       (((tIOError `fn` TAp tIOResult (TGen 0)) `fn`
                         (TGen 0 `fn` TAp tIOResult (TGen 0)) `fn`
                         TAp tIOResult (TGen 0)) `fn`
                          TAp tIO (TGen 0))))

tIOResult :: Type
tIOResult = TCon (Tycon "IOResult" (Kfun Star Star))
hugs_ExitWithCfun :: Assump
hugs_ExitWithCfun
          = "Hugs_ExitWith" :>: (Forall [Star]
                                 ([] :=> (tInt `fn` TAp tIOResult (TGen 0))))
hugs_SuspendThreadCfun :: Assump
hugs_SuspendThreadCfun
          = "Hugs_SuspendThread" :>: (Forall [Star]
                                      ([] :=> (TAp tIOResult (TGen 0))))
hugs_ErrorCfun :: Assump
hugs_ErrorCfun
          = "Hugs_Error" :>: (Forall [Star]
                              ([] :=> (tIOError `fn` TAp tIOResult (TGen 0))))
hugs_ReturnCfun :: Assump
hugs_ReturnCfun
          = "Hugs_Return" :>: (Forall [Star]
                               ([] :=> (TGen 0 `fn` TAp tIOResult (TGen 0))))

-----------------------------------------------------------------------------
-- Standard Classes and Member functions:
--
-- code to define these classes is provided in Preds.hs, so the code here
-- just defines the member functions and instances.

preludeClasses :: EnvTransformer
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

cEq :: [Char]
cEq = "Eq"

eqMfun :: Assump
eqMfun  = "==" :>: (Forall [Star]
                    ([isIn1 cEq (TGen 0)] :=>
                     (TGen 0 `fn` TGen 0 `fn` tBool)))
neqMfun :: Assump
neqMfun = "/=" :>: (Forall [Star]
                    ([isIn1 cEq (TGen 0)] :=>
                     (TGen 0 `fn` TGen 0 `fn` tBool)))

instsEq :: [Qual Pred]
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

cOrd :: [Char]
cOrd = "Ord"

compareMfun :: Assump
compareMfun
 = "compare" :>: (Forall [Star]
                   ([isIn1 cOrd (TGen 0)] :=>
                    (TGen 0 `fn` TGen 0 `fn` tOrdering)))
ltMfun :: Assump
ltMfun
 = "<" :>: (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` tBool)))
leMfun :: Assump
leMfun
 = "<=" :>: (Forall [Star]
              ([isIn1 cOrd (TGen 0)] :=>
               (TGen 0 `fn` TGen 0 `fn` tBool)))
geMfun :: Assump
geMfun
 = ">=" :>: (Forall [Star]
              ([isIn1 cOrd (TGen 0)] :=>
               (TGen 0 `fn` TGen 0 `fn` tBool)))
gtMfun :: Assump
gtMfun
 = ">" :>: (Forall [Star]
             ([isIn1 cOrd (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` tBool)))
maxMfun :: Assump
maxMfun
 = "max" :>: (Forall [Star]
               ([isIn1 cOrd (TGen 0)] :=>
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minMfun :: Assump
minMfun
 = "min" :>: (Forall [Star]
               ([isIn1 cOrd (TGen 0)] :=>
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))

instsOrd :: [Qual Pred]
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

cBounded :: [Char]
cBounded = "Bounded"

minBoundMfun :: Assump
minBoundMfun
 = "minBound" :>: (Forall [Star]
                    ([isIn1 cBounded (TGen 0)] :=>
                     (TGen 0)))
maxBoundMfun :: Assump
maxBoundMfun
 = "maxBound" :>: (Forall [Star]
                    ([isIn1 cBounded (TGen 0)] :=>
                     (TGen 0)))

instsBounded :: [Qual Pred]
instsBounded
 = [mkInst [] ([] :=> isIn1 cBounded tUnit),
    mkInst [] ([] :=> isIn1 cBounded tChar),
    mkInst [] ([] :=> isIn1 cBounded tInt),
    mkInst [] ([] :=> isIn1 cBounded tBool),
    mkInst [] ([] :=> isIn1 cBounded tOrdering)]

-- The Num class ------------------------------------------------------------

cNum :: [Char]
cNum = "Num"

plusMfun :: Assump
plusMfun
 = "+" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minusMfun :: Assump
minusMfun
 = "-" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
timesMfun :: Assump
timesMfun
 = "*" :>: (Forall [Star]
             ([isIn1 cNum (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
negateMfun :: Assump
negateMfun
 = "negate" :>: (Forall [Star]
                  ([isIn1 cNum (TGen 0)] :=>
                   (TGen 0 `fn` TGen 0)))
absMfun :: Assump
absMfun
 = "abs" :>: (Forall [Star]
               ([isIn1 cNum (TGen 0)] :=>
                (TGen 0 `fn` TGen 0)))
signumMfun :: Assump
signumMfun
 = "signum" :>: (Forall [Star]
                  ([isIn1 cNum (TGen 0)] :=>
                   (TGen 0 `fn` TGen 0)))
fromIntegerMfun :: Assump
fromIntegerMfun
 = "fromInteger" :>: (Forall [Star]
                       ([isIn1 cNum (TGen 0)] :=>
                        (tInteger `fn` TGen 0)))
fromIntMfun :: Assump
fromIntMfun
 = "fromInt" :>: (Forall [Star]
                   ([isIn1 cNum (TGen 0)] :=>
                    (tInt `fn` TGen 0)))

instsNum :: [Qual Pred]
instsNum
 = [mkInst [] ([] :=> isIn1 cNum tInt),
    mkInst [] ([] :=> isIn1 cNum tInteger),
    mkInst [] ([] :=> isIn1 cNum tFloat),
    mkInst [] ([] :=> isIn1 cNum tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=>
      isIn1 cNum (TAp tRatio (TGen 0)))]

-- The Real class -----------------------------------------------------------

cReal :: [Char]
cReal = "Real"

toRationalMfun :: Assump
toRationalMfun
 = "toRational" :>: (Forall [Star]
                      ([isIn1 cReal (TGen 0)] :=>
                       (TGen 0 `fn` tRational)))

instsReal :: [Qual Pred]
instsReal
 = [mkInst [] ([] :=> isIn1 cReal tInt),
    mkInst [] ([] :=> isIn1 cReal tInteger),
    mkInst [] ([] :=> isIn1 cReal tFloat),
    mkInst [] ([] :=> isIn1 cReal tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=>
      isIn1 cReal (TAp tRatio (TGen 0)))]

-- The Integral class -------------------------------------------------------

cIntegral :: [Char]
cIntegral = "Integral"

quotMfun :: Assump
quotMfun
 = "quot" :>: (Forall [Star]
                ([isIn1 cIntegral (TGen 0)] :=>
                 (TGen 0 `fn` TGen 0 `fn` TGen 0)))
remMfun :: Assump
remMfun
 = "rem" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=>
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
divMfun :: Assump
divMfun
 = "div" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=>
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
modMfun :: Assump
modMfun
 = "mod" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=>
                (TGen 0 `fn` TGen 0 `fn` TGen 0)))
quotRemMfun :: Assump
quotRemMfun
 = "quotRem" :>: (Forall [Star]
                   ([isIn1 cIntegral (TGen 0)] :=>
                    (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
divModMfun :: Assump
divModMfun
 = "divMod" :>: (Forall [Star]
                  ([isIn1 cIntegral (TGen 0)] :=>
                   (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
evenMfun :: Assump
evenMfun
 = "even" :>: (Forall [Star]
                ([isIn1 cIntegral (TGen 0)] :=>
                 (TGen 0 `fn` tBool)))
oddMfun :: Assump
oddMfun
 = "odd" :>: (Forall [Star]
               ([isIn1 cIntegral (TGen 0)] :=>
                (TGen 0 `fn` tBool)))
toIntegerMfun :: Assump
toIntegerMfun
 = "toInteger" :>: (Forall [Star]
                     ([isIn1 cIntegral (TGen 0)] :=>
                      (TGen 0 `fn` tInteger)))
toIntMfun :: Assump
toIntMfun
 = "toInt" :>: (Forall [Star]
                 ([isIn1 cIntegral (TGen 0)] :=>
                  (TGen 0 `fn` tInt)))

instsIntegral :: [Qual Pred]
instsIntegral
 = [mkInst [] ([] :=> isIn1 cIntegral tInt),
    mkInst [] ([] :=> isIn1 cIntegral tInteger)]

-- The Fractional class -----------------------------------------------------

cFractional :: [Char]
cFractional = "Fractional"

divideMfun :: Assump
divideMfun
 = "/" :>: (Forall [Star]
             ([isIn1 cFractional (TGen 0)] :=>
              (TGen 0 `fn` TGen 0 `fn` TGen 0)))
recipMfun :: Assump
recipMfun
 = "recip" :>: (Forall [Star]
                 ([isIn1 cFractional (TGen 0)] :=>
                  (TGen 0 `fn` TGen 0)))
fromRationalMfun :: Assump
fromRationalMfun
 = "fromRational" :>: (Forall [Star]
                        ([isIn1 cFractional (TGen 0)] :=>
                         (tRational `fn` TGen 0)))
fromDoubleMfun :: Assump
fromDoubleMfun
 = "fromDouble" :>: (Forall [Star]
                      ([isIn1 cFractional (TGen 0)] :=>
                       (tDouble `fn` TGen 0)))

instsFractional :: [Qual Pred]
instsFractional
 = [mkInst [] ([] :=> isIn1 cFractional tFloat),
    mkInst [] ([] :=> isIn1 cFractional tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=>
      isIn1 cFractional (TAp tRatio (TGen 0)))]

-- The Floating class -------------------------------------------------------

cFloating :: [Char]
cFloating = "Floating"

piMfun :: Assump
piMfun
 = "pi" :>: (Forall [Star]
              ([isIn1 cFloating (TGen 0)] :=> (TGen 0)))
expMfun :: Assump
expMfun
 = "exp" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
logMfun :: Assump
logMfun
 = "log" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sqrtMfun :: Assump
sqrtMfun
 = "sqrt" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
starstarMfun :: Assump
starstarMfun
 = "**" :>: (Forall [Star]
              ([isIn1 cFloating (TGen 0)] :=>
               (TGen 0 `fn` TGen 0 `fn` TGen 0)))
logBaseMfun :: Assump
logBaseMfun
 = "logBase" :>: (Forall [Star]
                   ([isIn1 cFloating (TGen 0)] :=>
                    (TGen 0 `fn` TGen 0 `fn` TGen 0)))
sinMfun :: Assump
sinMfun
 = "sin" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
cosMfun :: Assump
cosMfun
 = "cos" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanMfun :: Assump
tanMfun
 = "tan" :>: (Forall [Star]
               ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinMfun :: Assump
asinMfun
 = "asin" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acosMfun :: Assump
acosMfun
 = "acos" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanMfun :: Assump
atanMfun
 = "atan" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sinhMfun :: Assump
sinhMfun
 = "sinh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
coshMfun :: Assump
coshMfun
 = "cosh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanhMfun :: Assump
tanhMfun
 = "tanh" :>: (Forall [Star]
                ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinhMfun :: Assump
asinhMfun
 = "asinh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acoshMfun :: Assump
acoshMfun
 = "acosh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanhMfun :: Assump
atanhMfun
 = "atanh" :>: (Forall [Star]
                 ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))

instsFloating :: [Qual Pred]
instsFloating
 = [mkInst [] ([] :=> isIn1 cFloating tFloat),
    mkInst [] ([] :=> isIn1 cFloating tDouble)]

-- The RealFrac class -------------------------------------------------------

cRealFrac :: [Char]
cRealFrac = "RealFrac"

properFractionMfun :: Assump
properFractionMfun
 = "properFraction" :>: (Forall [Star, Star]
                          ([isIn1 cRealFrac (TGen 0),
                            isIn1 cIntegral (TGen 1)] :=>
                           (TGen 0 `fn` TAp (TAp tTuple2 (TGen 1)) (TGen 0))))
truncateMfun :: Assump
truncateMfun
 = "truncate" :>: (Forall [Star, Star]
                    ([isIn1 cRealFrac (TGen 0),
                      isIn1 cIntegral (TGen 1)] :=>
                     (TGen 0 `fn` TGen 1)))
roundMfun :: Assump
roundMfun
 = "round" :>: (Forall [Star, Star]
                 ([isIn1 cRealFrac (TGen 0),
                   isIn1 cIntegral (TGen 1)] :=>
                  (TGen 0 `fn` TGen 1)))
ceilingMfun :: Assump
ceilingMfun
 = "ceiling" :>: (Forall [Star, Star]
                   ([isIn1 cRealFrac (TGen 0),
                     isIn1 cIntegral (TGen 1)] :=>
                    (TGen 0 `fn` TGen 1)))
floorMfun :: Assump
floorMfun
 = "floor" :>: (Forall [Star, Star]
                 ([isIn1 cRealFrac (TGen 0),
                   isIn1 cIntegral (TGen 1)] :=>
                  (TGen 0 `fn` TGen 1)))

instsRealFrac :: [Qual Pred]
instsRealFrac
 = [mkInst [] ([] :=> isIn1 cRealFrac tFloat),
    mkInst [] ([] :=> isIn1 cRealFrac tDouble),
    mkInst [Star]
     ([isIn1 cIntegral (TGen 0)] :=> isIn1 cRealFrac (TAp tRatio (TGen 0)))]

-- The RealFloat class ------------------------------------------------------

cRealFloat :: [Char]
cRealFloat = "RealFloat"

floatRadixMfun :: Assump
floatRadixMfun
 = "floatRadix" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=>
                       (TGen 0 `fn` tInteger)))
floatDigitsMfun :: Assump
floatDigitsMfun
 = "floatDigits" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=>
                        (TGen 0 `fn` tInt)))
floatRangeMfun :: Assump
floatRangeMfun
 = "floatRange" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=>
                       (TGen 0 `fn` TAp (TAp tTuple2 tInt) tInt)))
decodeFloatMfun :: Assump
decodeFloatMfun
 = "decodeFloat" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=>
                        (TGen 0 `fn` TAp (TAp tTuple2 tInteger) tInt)))
encodeFloatMfun :: Assump
encodeFloatMfun
 = "encodeFloat" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=>
                        (tInteger `fn` tInt `fn` TGen 0)))
exponentMfun :: Assump
exponentMfun
 = "exponent" :>: (Forall [Star]
                    ([isIn1 cRealFloat (TGen 0)] :=>
                     (TGen 0 `fn` tInt)))
significandMfun :: Assump
significandMfun
 = "significand" :>: (Forall [Star]
                       ([isIn1 cRealFloat (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0)))
scaleFloatMfun :: Assump
scaleFloatMfun
 = "scaleFloat" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=>
                       (tInt `fn` TGen 0 `fn` TGen 0)))
isNaNMfun :: Assump
isNaNMfun
 = "isNaN" :>: (Forall [Star]
                 ([isIn1 cRealFloat (TGen 0)] :=>
                  (TGen 0 `fn` tBool)))
isInfiniteMfun :: Assump
isInfiniteMfun
 = "isInfinite" :>: (Forall [Star]
                      ([isIn1 cRealFloat (TGen 0)] :=>
                       (TGen 0 `fn` tBool)))
isDenormalizedMfun :: Assump
isDenormalizedMfun
 = "isDenormalized" :>: (Forall [Star]
                          ([isIn1 cRealFloat (TGen 0)] :=>
                           (TGen 0 `fn` tBool)))
isNegativeZeroMfun :: Assump
isNegativeZeroMfun
 = "isNegativeZero" :>: (Forall [Star]
                          ([isIn1 cRealFloat (TGen 0)] :=>
                           (TGen 0 `fn` tBool)))
isIEEEMfun :: Assump
isIEEEMfun
 = "isIEEE" :>: (Forall [Star]
                  ([isIn1 cRealFloat (TGen 0)] :=>
                   (TGen 0 `fn` tBool)))
atan2Mfun :: Assump
atan2Mfun
 = "atan2" :>: (Forall [Star]
                 ([isIn1 cRealFloat (TGen 0)] :=>
                  (TGen 0 `fn` TGen 0 `fn` TGen 0)))

instsRealFloat :: [Qual Pred]
instsRealFloat
 = [mkInst [] ([] :=> isIn1 cRealFloat tFloat),
    mkInst [] ([] :=> isIn1 cRealFloat tDouble)]

-- The Enum class -----------------------------------------------------------

cEnum :: [Char]
cEnum = "Enum"

succMfun :: Assump
succMfun
 = "succ" :>: (Forall [Star]
                ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
predMfun :: Assump
predMfun
 = "pred" :>: (Forall [Star]
                ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
toEnumMfun :: Assump
toEnumMfun
 = "toEnum" :>: (Forall [Star]
                  ([isIn1 cEnum (TGen 0)] :=> (tInt `fn` TGen 0)))
fromEnumMfun :: Assump
fromEnumMfun
 = "fromEnum" :>: (Forall [Star]
                    ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` tInt)))
enumFromMfun :: Assump
enumFromMfun
 = "enumFrom" :>: (Forall [Star]
                    ([isIn1 cEnum (TGen 0)] :=>
                     (TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenMfun :: Assump
enumFromThenMfun
 = "enumFromThen" :>: (Forall [Star]
                        ([isIn1 cEnum (TGen 0)] :=>
                         (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromToMfun :: Assump
enumFromToMfun
 = "enumFromTo" :>: (Forall [Star]
                      ([isIn1 cEnum (TGen 0)] :=>
                       (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenToMfun :: Assump
enumFromThenToMfun
 = "enumFromThenTo" :>: (Forall [Star]
                          ([isIn1 cEnum (TGen 0)] :=>
                           (TGen 0 `fn` TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))

instsEnum :: [Qual Pred]
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

cRead :: [Char]
cRead = "Read"

readsPrecMfun :: Assump
readsPrecMfun
 = "readsPrec" :>: (Forall [Star]
                     ([isIn1 cRead (TGen 0)] :=> (tInt `fn` tReadS (TGen 0))))
readListMfun :: Assump
readListMfun
 = "readList" :>: (Forall [Star]
                    ([isIn1 cRead (TGen 0)] :=> tReadS (TAp tList (TGen 0))))

instsRead :: [Qual Pred]
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

cShow :: [Char]
cShow = "Show"

showMfun :: Assump
showMfun
 = "show" :>: (Forall [Star]
                ([isIn1 cShow (TGen 0)] :=>
                 (TGen 0 `fn` tString)))
showsPrecMfun :: Assump
showsPrecMfun
 = "showsPrec" :>: (Forall [Star]
                     ([isIn1 cShow (TGen 0)] :=>
                      (tInt `fn` TGen 0 `fn` tShowS)))
showListMfun :: Assump
showListMfun
 = "showList" :>: (Forall [Star]
                    ([isIn1 cShow (TGen 0)] :=>
                     (TAp tList (TGen 0) `fn` tShowS)))

instsShow :: [Qual Pred]
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

cIx :: [Char]
cIx = "Ix"

rangeMfun :: Assump
rangeMfun
 = "range" :>: (Forall [Star]
                ([isIn1 cIx (TGen 0)] :=>
                 (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                  TAp tList (TGen 0))))
indexMfun :: Assump
indexMfun
 = "index" :>: (Forall [Star]
                ([isIn1 cIx (TGen 0)] :=>
                 (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                  TGen 0 `fn` tInt)))
inRangeMfun :: Assump
inRangeMfun
 = "inRange" :>: (Forall [Star]
                  ([isIn1 cIx (TGen 0)] :=>
                   (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                    TGen 0 `fn` tBool)))
rangeSizeMfun :: Assump
rangeSizeMfun
 = "rangeSize" :>: (Forall [Star]
                    ([isIn1 cIx (TGen 0)] :=>
                     (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` tInt)))

instsIx :: [Qual Pred]
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

cFunctor :: [Char]
cFunctor = "Functor"

fmapMfun :: Assump
fmapMfun
 = "fmap" :>: (Forall [Kfun Star Star, Star, Star]
                ([isIn1 cFunctor (TGen 0)] :=>
                 ((TGen 1 `fn` TGen 2) `fn`
                  TAp (TGen 0) (TGen 1) `fn`
                  TAp (TGen 0) (TGen 2))))

instsFunctor :: [Qual Pred]
instsFunctor
 = [mkInst [] ([] :=> isIn1 cFunctor tMaybe),
    mkInst [] ([] :=> isIn1 cFunctor tList),
    mkInst [] ([] :=> isIn1 cFunctor tIO)]

-- The Monad class ----------------------------------------------------------

cMonad :: [Char]
cMonad = "Monad"

returnMfun :: Assump
returnMfun
 = "return" :>: (Forall [Kfun Star Star, Star]
                  ([isIn1 cMonad (TGen 0)] :=>
                   (TGen 1 `fn` TAp (TGen 0) (TGen 1))))
mbindMfun :: Assump
mbindMfun
 = ">>=" :>: (Forall [Kfun Star Star, Star, Star]
               ([isIn1 cMonad (TGen 0)] :=>
                (TAp (TGen 0) (TGen 1) `fn` (TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 2))))
mthenMfun :: Assump
mthenMfun
 = ">>" :>: (Forall [Kfun Star Star, Star, Star]
              ([isIn1 cMonad (TGen 0)] :=>
               (TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 2))))
failMfun :: Assump
failMfun
 = "fail" :>: (Forall [Kfun Star Star, Star]
                ([isIn1 cMonad (TGen 0)] :=>
                 (tString `fn` TAp (TGen 0) (TGen 1))))

instsMonad :: [Qual Pred]
instsMonad
 = [mkInst [] ([] :=> isIn1 cMonad tMaybe),
    mkInst [] ([] :=> isIn1 cMonad tList),
    mkInst [] ([] :=> isIn1 cMonad tIO)]

-----------------------------------------------------------------------------
