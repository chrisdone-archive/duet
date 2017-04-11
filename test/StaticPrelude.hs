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
import THIH

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
-- Type assumptiontions for the constructors of these types are provided below:

unitCfun :: Assumption
unitCfun = Assumption "()"  (Forall [] ([] :=> tUnit))

nilCfun :: Assumption
nilCfun  = Assumption "[]"  (Forall [Star] ([] :=> (TAp tList (TGen 0))))
consCfun :: Assumption
consCfun = Assumption ":"   (Forall [Star]
                             ([] :=>
                              (TGen 0 `fn`
                               TAp tList (TGen 0) `fn`
                               TAp tList (TGen 0))))

tup2Cfun :: Assumption
tup2Cfun = Assumption "(,)"  (Forall [Star, Star]
                              ([] :=>
                               (TGen 0 `fn`
                                TGen 1 `fn`
                                foldl TAp tTuple2 (map TGen [0..1]))))

tup3Cfun :: Assumption
tup3Cfun = Assumption "(,,)"  (Forall [Star, Star, Star]
                               ([] :=>
                                (TGen 0 `fn`
                                 TGen 1 `fn`
                                 TGen 2 `fn`
                                 foldl TAp tTuple3 (map TGen [0..2]))))

tup4Cfun :: Assumption
tup4Cfun = Assumption "(,,,)"  (Forall [Star, Star, Star, Star]
                                ([] :=>
                                 (TGen 0 `fn`
                                  TGen 1 `fn`
                                  TGen 2 `fn`
                                  TGen 3 `fn`
                                  foldl TAp tTuple4 (map TGen [0..3]))))

tup5Cfun :: Assumption
tup5Cfun = Assumption "(,,,,)"  (Forall [Star, Star, Star, Star, Star]
                                 ([] :=>
                                  (TGen 0 `fn`
                                   TGen 1 `fn`
                                   TGen 2 `fn`
                                   TGen 3 `fn`
                                   TGen 4 `fn`
                                   foldl TAp tTuple5 (map TGen [0..4]))))

tup6Cfun :: Assumption
tup6Cfun = Assumption "(,,,,,)"  (Forall [Star, Star, Star, Star, Star, Star]
                                  ([] :=>
                                   (TGen 0 `fn`
                                    TGen 1 `fn`
                                    TGen 2 `fn`
                                    TGen 3 `fn`
                                    TGen 4 `fn`
                                    TGen 5 `fn`
                                    foldl TAp tTuple6 (map TGen [0..5]))))

tup7Cfun :: Assumption
tup7Cfun = Assumption "(,,,,,,)"  (Forall [Star, Star, Star, Star, Star, Star, Star]
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


falseCfun :: Assumption
falseCfun = Assumption "False"  (Forall [] ([] :=> tBool))
trueCfun :: Assumption
trueCfun  = Assumption "True"  (Forall [] ([] :=> tBool))

tMaybe :: Type
tMaybe      = TCon (Tycon "Maybe" (Kfun Star Star))
nothingCfun :: Assumption
nothingCfun = Assumption "Nothing"  (Forall [Star]
                                     ([] :=> (TAp tMaybe (TGen 0))))
justCfun :: Assumption
justCfun    = Assumption "Just"  (Forall [Star]
                                  ([] :=> (TGen 0 `fn` TAp tMaybe (TGen 0))))

tEither :: Type
tEither   = TCon (Tycon "Either" (Kfun Star (Kfun Star Star)))
leftCfun :: Assumption
leftCfun  = Assumption "Left"  (Forall [Star, Star]
                                ([] :=>
                                 (TGen 0 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))
rightCfun :: Assumption
rightCfun = Assumption "Right"  (Forall [Star, Star]
                                 ([] :=>
                                  (TGen 1 `fn` TAp (TAp tEither (TGen 0)) (TGen 1))))

tOrdering :: Type
tOrdering = TCon (Tycon "Ordering" Star)
lTCfun :: Assumption
lTCfun    = Assumption "LT"  (Forall [] ([] :=> tOrdering))
eQCfun :: Assumption
eQCfun    = Assumption "EQ"  (Forall [] ([] :=> tOrdering))
gTCfun :: Assumption
gTCfun    = Assumption "GT"  (Forall [] ([] :=> tOrdering))

tRatio :: Type
tRatio    = TCon (Tycon "Ratio" (Kfun Star Star))
consratCfun :: Assumption
consratCfun = Assumption ":%"  (Forall [Star]
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
iOCfun :: Assumption
iOCfun    = Assumption "IO"  (Forall [Star]
                              ([] :=>
                               (((tIOError `fn` TAp tIOResult (TGen 0)) `fn`
                                 (TGen 0 `fn` TAp tIOResult (TGen 0)) `fn`
                                 TAp tIOResult (TGen 0)) `fn`
                                  TAp tIO (TGen 0))))

tIOResult :: Type
tIOResult = TCon (Tycon "IOResult" (Kfun Star Star))
hugs_ExitWithCfun :: Assumption
hugs_ExitWithCfun
          = Assumption "Hugs_ExitWith"  (Forall [Star]
                                         ([] :=> (tInt `fn` TAp tIOResult (TGen 0))))
hugs_SuspendThreadCfun :: Assumption
hugs_SuspendThreadCfun
          = Assumption "Hugs_SuspendThread"  (Forall [Star]
                                              ([] :=> (TAp tIOResult (TGen 0))))
hugs_ErrorCfun :: Assumption
hugs_ErrorCfun
          = Assumption "Hugs_Error"  (Forall [Star]
                                      ([] :=> (tIOError `fn` TAp tIOResult (TGen 0))))
hugs_ReturnCfun :: Assumption
hugs_ReturnCfun
          = Assumption "Hugs_Return"  (Forall [Star]
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

eqMfun :: Assumption
eqMfun  = Assumption "=="  (Forall [Star]
                            ([isIn1 cEq (TGen 0)] :=>
                             (TGen 0 `fn` TGen 0 `fn` tBool)))
neqMfun :: Assumption
neqMfun = Assumption "/="  (Forall [Star]
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

compareMfun :: Assumption
compareMfun
 = Assumption "compare"  (Forall [Star]
                           ([isIn1 cOrd (TGen 0)] :=>
                            (TGen 0 `fn` TGen 0 `fn` tOrdering)))
ltMfun :: Assumption
ltMfun
 = Assumption "<"  (Forall [Star]
                     ([isIn1 cOrd (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` tBool)))
leMfun :: Assumption
leMfun
 = Assumption "<="  (Forall [Star]
                      ([isIn1 cOrd (TGen 0)] :=>
                       (TGen 0 `fn` TGen 0 `fn` tBool)))
geMfun :: Assumption
geMfun
 = Assumption ">="  (Forall [Star]
                      ([isIn1 cOrd (TGen 0)] :=>
                       (TGen 0 `fn` TGen 0 `fn` tBool)))
gtMfun :: Assumption
gtMfun
 = Assumption ">"  (Forall [Star]
                     ([isIn1 cOrd (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` tBool)))
maxMfun :: Assumption
maxMfun
 = Assumption "max"  (Forall [Star]
                       ([isIn1 cOrd (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minMfun :: Assumption
minMfun
 = Assumption "min"  (Forall [Star]
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

minBoundMfun :: Assumption
minBoundMfun
 = Assumption "minBound"  (Forall [Star]
                            ([isIn1 cBounded (TGen 0)] :=>
                             (TGen 0)))
maxBoundMfun :: Assumption
maxBoundMfun
 = Assumption "maxBound"  (Forall [Star]
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

plusMfun :: Assumption
plusMfun
 = Assumption "+"  (Forall [Star]
                     ([isIn1 cNum (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` TGen 0)))
minusMfun :: Assumption
minusMfun
 = Assumption "-"  (Forall [Star]
                     ([isIn1 cNum (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` TGen 0)))
timesMfun :: Assumption
timesMfun
 = Assumption "*"  (Forall [Star]
                     ([isIn1 cNum (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` TGen 0)))
negateMfun :: Assumption
negateMfun
 = Assumption "negate"  (Forall [Star]
                          ([isIn1 cNum (TGen 0)] :=>
                           (TGen 0 `fn` TGen 0)))
absMfun :: Assumption
absMfun
 = Assumption "abs"  (Forall [Star]
                       ([isIn1 cNum (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0)))
signumMfun :: Assumption
signumMfun
 = Assumption "signum"  (Forall [Star]
                          ([isIn1 cNum (TGen 0)] :=>
                           (TGen 0 `fn` TGen 0)))
fromIntegerMfun :: Assumption
fromIntegerMfun
 = Assumption "fromInteger"  (Forall [Star]
                               ([isIn1 cNum (TGen 0)] :=>
                                (tInteger `fn` TGen 0)))
fromIntMfun :: Assumption
fromIntMfun
 = Assumption "fromInt"  (Forall [Star]
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

toRationalMfun :: Assumption
toRationalMfun
 = Assumption "toRational"  (Forall [Star]
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

quotMfun :: Assumption
quotMfun
 = Assumption "quot"  (Forall [Star]
                        ([isIn1 cIntegral (TGen 0)] :=>
                         (TGen 0 `fn` TGen 0 `fn` TGen 0)))
remMfun :: Assumption
remMfun
 = Assumption "rem"  (Forall [Star]
                       ([isIn1 cIntegral (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0 `fn` TGen 0)))
divMfun :: Assumption
divMfun
 = Assumption "div"  (Forall [Star]
                       ([isIn1 cIntegral (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0 `fn` TGen 0)))
modMfun :: Assumption
modMfun
 = Assumption "mod"  (Forall [Star]
                       ([isIn1 cIntegral (TGen 0)] :=>
                        (TGen 0 `fn` TGen 0 `fn` TGen 0)))
quotRemMfun :: Assumption
quotRemMfun
 = Assumption "quotRem"  (Forall [Star]
                           ([isIn1 cIntegral (TGen 0)] :=>
                            (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
divModMfun :: Assumption
divModMfun
 = Assumption "divMod"  (Forall [Star]
                          ([isIn1 cIntegral (TGen 0)] :=>
                           (TGen 0 `fn` TGen 0 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 0))))
evenMfun :: Assumption
evenMfun
 = Assumption "even"  (Forall [Star]
                        ([isIn1 cIntegral (TGen 0)] :=>
                         (TGen 0 `fn` tBool)))
oddMfun :: Assumption
oddMfun
 = Assumption "odd"  (Forall [Star]
                       ([isIn1 cIntegral (TGen 0)] :=>
                        (TGen 0 `fn` tBool)))
toIntegerMfun :: Assumption
toIntegerMfun
 = Assumption "toInteger"  (Forall [Star]
                             ([isIn1 cIntegral (TGen 0)] :=>
                              (TGen 0 `fn` tInteger)))
toIntMfun :: Assumption
toIntMfun
 = Assumption "toInt"  (Forall [Star]
                         ([isIn1 cIntegral (TGen 0)] :=>
                          (TGen 0 `fn` tInt)))

instsIntegral :: [Qual Pred]
instsIntegral
 = [mkInst [] ([] :=> isIn1 cIntegral tInt),
    mkInst [] ([] :=> isIn1 cIntegral tInteger)]

-- The Fractional class -----------------------------------------------------

cFractional :: [Char]
cFractional = "Fractional"

divideMfun :: Assumption
divideMfun
 = Assumption "/"  (Forall [Star]
                     ([isIn1 cFractional (TGen 0)] :=>
                      (TGen 0 `fn` TGen 0 `fn` TGen 0)))
recipMfun :: Assumption
recipMfun
 = Assumption "recip"  (Forall [Star]
                         ([isIn1 cFractional (TGen 0)] :=>
                          (TGen 0 `fn` TGen 0)))
fromRationalMfun :: Assumption
fromRationalMfun
 = Assumption "fromRational"  (Forall [Star]
                                ([isIn1 cFractional (TGen 0)] :=>
                                 (tRational `fn` TGen 0)))
fromDoubleMfun :: Assumption
fromDoubleMfun
 = Assumption "fromDouble"  (Forall [Star]
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

piMfun :: Assumption
piMfun
 = Assumption "pi"  (Forall [Star]
                      ([isIn1 cFloating (TGen 0)] :=> (TGen 0)))
expMfun :: Assumption
expMfun
 = Assumption "exp"  (Forall [Star]
                       ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
logMfun :: Assumption
logMfun
 = Assumption "log"  (Forall [Star]
                       ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sqrtMfun :: Assumption
sqrtMfun
 = Assumption "sqrt"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
starstarMfun :: Assumption
starstarMfun
 = Assumption "**"  (Forall [Star]
                      ([isIn1 cFloating (TGen 0)] :=>
                       (TGen 0 `fn` TGen 0 `fn` TGen 0)))
logBaseMfun :: Assumption
logBaseMfun
 = Assumption "logBase"  (Forall [Star]
                           ([isIn1 cFloating (TGen 0)] :=>
                            (TGen 0 `fn` TGen 0 `fn` TGen 0)))
sinMfun :: Assumption
sinMfun
 = Assumption "sin"  (Forall [Star]
                       ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
cosMfun :: Assumption
cosMfun
 = Assumption "cos"  (Forall [Star]
                       ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanMfun :: Assumption
tanMfun
 = Assumption "tan"  (Forall [Star]
                       ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinMfun :: Assumption
asinMfun
 = Assumption "asin"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acosMfun :: Assumption
acosMfun
 = Assumption "acos"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanMfun :: Assumption
atanMfun
 = Assumption "atan"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
sinhMfun :: Assumption
sinhMfun
 = Assumption "sinh"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
coshMfun :: Assumption
coshMfun
 = Assumption "cosh"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
tanhMfun :: Assumption
tanhMfun
 = Assumption "tanh"  (Forall [Star]
                        ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
asinhMfun :: Assumption
asinhMfun
 = Assumption "asinh"  (Forall [Star]
                         ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
acoshMfun :: Assumption
acoshMfun
 = Assumption "acosh"  (Forall [Star]
                         ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
atanhMfun :: Assumption
atanhMfun
 = Assumption "atanh"  (Forall [Star]
                         ([isIn1 cFloating (TGen 0)] :=> (TGen 0 `fn` TGen 0)))

instsFloating :: [Qual Pred]
instsFloating
 = [mkInst [] ([] :=> isIn1 cFloating tFloat),
    mkInst [] ([] :=> isIn1 cFloating tDouble)]

-- The RealFrac class -------------------------------------------------------

cRealFrac :: [Char]
cRealFrac = "RealFrac"

properFractionMfun :: Assumption
properFractionMfun
 = Assumption "properFraction"  (Forall [Star, Star]
                                  ([isIn1 cRealFrac (TGen 0),
                                    isIn1 cIntegral (TGen 1)] :=>
                                   (TGen 0 `fn` TAp (TAp tTuple2 (TGen 1)) (TGen 0))))
truncateMfun :: Assumption
truncateMfun
 = Assumption "truncate"  (Forall [Star, Star]
                            ([isIn1 cRealFrac (TGen 0),
                              isIn1 cIntegral (TGen 1)] :=>
                             (TGen 0 `fn` TGen 1)))
roundMfun :: Assumption
roundMfun
 = Assumption "round"  (Forall [Star, Star]
                         ([isIn1 cRealFrac (TGen 0),
                           isIn1 cIntegral (TGen 1)] :=>
                          (TGen 0 `fn` TGen 1)))
ceilingMfun :: Assumption
ceilingMfun
 = Assumption "ceiling"  (Forall [Star, Star]
                           ([isIn1 cRealFrac (TGen 0),
                             isIn1 cIntegral (TGen 1)] :=>
                            (TGen 0 `fn` TGen 1)))
floorMfun :: Assumption
floorMfun
 = Assumption "floor"  (Forall [Star, Star]
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

floatRadixMfun :: Assumption
floatRadixMfun
 = Assumption "floatRadix"  (Forall [Star]
                              ([isIn1 cRealFloat (TGen 0)] :=>
                               (TGen 0 `fn` tInteger)))
floatDigitsMfun :: Assumption
floatDigitsMfun
 = Assumption "floatDigits"  (Forall [Star]
                               ([isIn1 cRealFloat (TGen 0)] :=>
                                (TGen 0 `fn` tInt)))
floatRangeMfun :: Assumption
floatRangeMfun
 = Assumption "floatRange"  (Forall [Star]
                              ([isIn1 cRealFloat (TGen 0)] :=>
                               (TGen 0 `fn` TAp (TAp tTuple2 tInt) tInt)))
decodeFloatMfun :: Assumption
decodeFloatMfun
 = Assumption "decodeFloat"  (Forall [Star]
                               ([isIn1 cRealFloat (TGen 0)] :=>
                                (TGen 0 `fn` TAp (TAp tTuple2 tInteger) tInt)))
encodeFloatMfun :: Assumption
encodeFloatMfun
 = Assumption "encodeFloat"  (Forall [Star]
                               ([isIn1 cRealFloat (TGen 0)] :=>
                                (tInteger `fn` tInt `fn` TGen 0)))
exponentMfun :: Assumption
exponentMfun
 = Assumption "exponent"  (Forall [Star]
                            ([isIn1 cRealFloat (TGen 0)] :=>
                             (TGen 0 `fn` tInt)))
significandMfun :: Assumption
significandMfun
 = Assumption "significand"  (Forall [Star]
                               ([isIn1 cRealFloat (TGen 0)] :=>
                                (TGen 0 `fn` TGen 0)))
scaleFloatMfun :: Assumption
scaleFloatMfun
 = Assumption "scaleFloat"  (Forall [Star]
                              ([isIn1 cRealFloat (TGen 0)] :=>
                               (tInt `fn` TGen 0 `fn` TGen 0)))
isNaNMfun :: Assumption
isNaNMfun
 = Assumption "isNaN"  (Forall [Star]
                         ([isIn1 cRealFloat (TGen 0)] :=>
                          (TGen 0 `fn` tBool)))
isInfiniteMfun :: Assumption
isInfiniteMfun
 = Assumption "isInfinite"  (Forall [Star]
                              ([isIn1 cRealFloat (TGen 0)] :=>
                               (TGen 0 `fn` tBool)))
isDenormalizedMfun :: Assumption
isDenormalizedMfun
 = Assumption "isDenormalized"  (Forall [Star]
                                  ([isIn1 cRealFloat (TGen 0)] :=>
                                   (TGen 0 `fn` tBool)))
isNegativeZeroMfun :: Assumption
isNegativeZeroMfun
 = Assumption "isNegativeZero"  (Forall [Star]
                                  ([isIn1 cRealFloat (TGen 0)] :=>
                                   (TGen 0 `fn` tBool)))
isIEEEMfun :: Assumption
isIEEEMfun
 = Assumption "isIEEE"  (Forall [Star]
                          ([isIn1 cRealFloat (TGen 0)] :=>
                           (TGen 0 `fn` tBool)))
atan2Mfun :: Assumption
atan2Mfun
 = Assumption "atan2"  (Forall [Star]
                         ([isIn1 cRealFloat (TGen 0)] :=>
                          (TGen 0 `fn` TGen 0 `fn` TGen 0)))

instsRealFloat :: [Qual Pred]
instsRealFloat
 = [mkInst [] ([] :=> isIn1 cRealFloat tFloat),
    mkInst [] ([] :=> isIn1 cRealFloat tDouble)]

-- The Enum class -----------------------------------------------------------

cEnum :: [Char]
cEnum = "Enum"

succMfun :: Assumption
succMfun
 = Assumption "succ"  (Forall [Star]
                        ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
predMfun :: Assumption
predMfun
 = Assumption "pred"  (Forall [Star]
                        ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` TGen 0)))
toEnumMfun :: Assumption
toEnumMfun
 = Assumption "toEnum"  (Forall [Star]
                          ([isIn1 cEnum (TGen 0)] :=> (tInt `fn` TGen 0)))
fromEnumMfun :: Assumption
fromEnumMfun
 = Assumption "fromEnum"  (Forall [Star]
                            ([isIn1 cEnum (TGen 0)] :=> (TGen 0 `fn` tInt)))
enumFromMfun :: Assumption
enumFromMfun
 = Assumption "enumFrom"  (Forall [Star]
                            ([isIn1 cEnum (TGen 0)] :=>
                             (TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenMfun :: Assumption
enumFromThenMfun
 = Assumption "enumFromThen"  (Forall [Star]
                                ([isIn1 cEnum (TGen 0)] :=>
                                 (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromToMfun :: Assumption
enumFromToMfun
 = Assumption "enumFromTo"  (Forall [Star]
                              ([isIn1 cEnum (TGen 0)] :=>
                               (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))))
enumFromThenToMfun :: Assumption
enumFromThenToMfun
 = Assumption "enumFromThenTo"  (Forall [Star]
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

readsPrecMfun :: Assumption
readsPrecMfun
 = Assumption "readsPrec"  (Forall [Star]
                             ([isIn1 cRead (TGen 0)] :=> (tInt `fn` tReadS (TGen 0))))
readListMfun :: Assumption
readListMfun
 = Assumption "readList"  (Forall [Star]
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

showMfun :: Assumption
showMfun
 = Assumption "show"  (Forall [Star]
                        ([isIn1 cShow (TGen 0)] :=>
                         (TGen 0 `fn` tString)))
showsPrecMfun :: Assumption
showsPrecMfun
 = Assumption "showsPrec"  (Forall [Star]
                             ([isIn1 cShow (TGen 0)] :=>
                              (tInt `fn` TGen 0 `fn` tShowS)))
showListMfun :: Assumption
showListMfun
 = Assumption "showList"  (Forall [Star]
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

rangeMfun :: Assumption
rangeMfun
 = Assumption "range"  (Forall [Star]
                        ([isIn1 cIx (TGen 0)] :=>
                         (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                          TAp tList (TGen 0))))
indexMfun :: Assumption
indexMfun
 = Assumption "index"  (Forall [Star]
                        ([isIn1 cIx (TGen 0)] :=>
                         (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                          TGen 0 `fn` tInt)))
inRangeMfun :: Assumption
inRangeMfun
 = Assumption "inRange"  (Forall [Star]
                          ([isIn1 cIx (TGen 0)] :=>
                           (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn`
                            TGen 0 `fn` tBool)))
rangeSizeMfun :: Assumption
rangeSizeMfun
 = Assumption "rangeSize"  (Forall [Star]
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

fmapMfun :: Assumption
fmapMfun
 = Assumption "fmap"  (Forall [Kfun Star Star, Star, Star]
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

returnMfun :: Assumption
returnMfun
 = Assumption "return"  (Forall [Kfun Star Star, Star]
                          ([isIn1 cMonad (TGen 0)] :=>
                           (TGen 1 `fn` TAp (TGen 0) (TGen 1))))
mbindMfun :: Assumption
mbindMfun
 = Assumption ">>="  (Forall [Kfun Star Star, Star, Star]
                       ([isIn1 cMonad (TGen 0)] :=>
                        (TAp (TGen 0) (TGen 1) `fn` (TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 2))))
mthenMfun :: Assumption
mthenMfun
 = Assumption ">>"  (Forall [Kfun Star Star, Star, Star]
                      ([isIn1 cMonad (TGen 0)] :=>
                       (TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 2))))
failMfun :: Assumption
failMfun
 = Assumption "fail"  (Forall [Kfun Star Star, Star]
                        ([isIn1 cMonad (TGen 0)] :=>
                         (tString `fn` TAp (TGen 0) (TGen 1))))

instsMonad :: [Qual Pred]
instsMonad
 = [mkInst [] ([] :=> isIn1 cMonad tMaybe),
    mkInst [] ([] :=> isIn1 cMonad tList),
    mkInst [] ([] :=> isIn1 cMonad tIO)]

-----------------------------------------------------------------------------
