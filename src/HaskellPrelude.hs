-- Automatically generated typing assumptions for Prelude

module HaskellPrelude where
import Testbed
import StaticPrelude

defnsHaskellPrelude
 =  ["flip" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TGen 1 `fn` TGen 0 `fn` TGen 2)),
     "subtract" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TGen 0)),
     "gcd" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TGen 0)),
     "lcm" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TGen 0)),
     "otherwise" :>:
       Forall []
	 ([] :=>
	    tBool),
     "^" :>:
       Forall [Star, Star]
	 ([isIn1 cNum (TGen 0), isIn1 cIntegral (TGen 1)] :=>
	    (TGen 0 `fn` TGen 1 `fn` TGen 0)),
     "^^" :>:
       Forall [Star, Star]
	 ([isIn1 cFractional (TGen 0), isIn1 cIntegral (TGen 1)] :=>
	    (TGen 0 `fn` TGen 1 `fn` TGen 0)),
     "." :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1) `fn` (TGen 2 `fn` TGen 0) `fn` TGen 2 `fn` TGen 1)),
     "fromIntegral" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0), isIn1 cNum (TGen 1)] :=>
	    (TGen 0 `fn` TGen 1)),
     "realToFrac" :>:
       Forall [Star, Star]
	 ([isIn1 cReal (TGen 0), isIn1 cFractional (TGen 1)] :=>
	    (TGen 0 `fn` TGen 1)),
     "sequence" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TAp tList (TGen 1)))),
     "foldr" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` TAp tList (TGen 0) `fn` TGen 1)),
     "sequence_" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) tUnit)),
     "map" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1))),
     "mapM" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp tList (TGen 2)))),
     "mapM_" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) tUnit)),
     "=<<" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2))),
     "&&" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` tBool `fn` tBool)),
     "||" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` tBool `fn` tBool)),
     "not" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` tBool)),
     "isAscii" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isControl" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isPrint" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isSpace" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isUpper" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isLower" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isAlpha" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isDigit" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isAlphaNum" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "digitToInt" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tInt)),
     "intToDigit" :>:
       Forall []
	 ([] :=>
	    (tInt `fn` tChar)),
     "toUpper" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tChar)),
     "toLower" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tChar)),
     "ord" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tInt)),
     "chr" :>:
       Forall []
	 ([] :=>
	    (tInt `fn` tChar)),
     "maybe" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TGen 0 `fn` (TGen 1 `fn` TGen 0) `fn` TAp tMaybe (TGen 1) `fn` TGen 0)),
     "either" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1) `fn` (TGen 2 `fn` TGen 1) `fn` TAp (TAp tEither (TGen 0)) (TGen 2) `fn` TGen 1)),
     "absReal" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0), isIn1 cOrd (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0)),
     "signumReal" :>:
       Forall [Star, Star]
	 ([isIn1 cNum (TGen 0), isIn1 cNum (TGen 1),
	   isIn1 cOrd (TGen 0)] :=>
	    (TGen 0 `fn` TGen 1)),
     "numericEnumFrom" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0))),
     "iterate" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 0))),
     "numericEnumFromThen" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))),
     "takeWhile" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "numericEnumFromTo" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))),
     "numericEnumFromThenTo" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0))),
     "reduce" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0))),
     "%" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TAp tRatio (TGen 0))),
     "realFloatToRational" :>:
       Forall [Star]
	 ([isIn1 cRealFloat (TGen 0)] :=>
	    (TGen 0 `fn` TAp tRatio tInteger)),
     "floatToRational" :>:
       Forall []
	 ([] :=>
	    (tFloat `fn` TAp tRatio tInteger)),
     "doubleToRational" :>:
       Forall []
	 ([] :=>
	    (tDouble `fn` TAp tRatio tInteger)),
     "const" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TGen 0 `fn` TGen 1 `fn` TGen 0)),
     "asTypeOf" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0 `fn` TGen 0 `fn` TGen 0)),
     "numerator" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tRatio (TGen 0) `fn` TGen 0)),
     "denominator" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tRatio (TGen 0) `fn` TGen 0)),
     "rationalToRealFloat" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tRatio tInteger `fn` TGen 0)),
     "rationalToFloat" :>:
       Forall []
	 ([] :=>
	    (TAp tRatio tInteger `fn` tFloat)),
     "rationalToDouble" :>:
       Forall []
	 ([] :=>
	    (TAp tRatio tInteger `fn` tDouble)),
     "floatProperFraction" :>:
       Forall [Star, Star, Star]
	 ([isIn1 cRealFloat (TGen 0), isIn1 cNum (TGen 1),
	   isIn1 cRealFloat (TGen 2)] :=>
	    (TGen 2 `fn` TAp (TAp tTuple2 (TGen 1)) (TGen 0))),
     "fst" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 0)),
     "snd" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 1)),
     "curry" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 2) `fn` TGen 0 `fn` TGen 1 `fn` TGen 2)),
     "uncurry" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 2)),
     "id" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0 `fn` TGen 0)),
     "$" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1) `fn` TGen 0 `fn` TGen 1)),
     "until" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` (TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` TGen 0)),
     "undefined" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0)),
     "intToRatio" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (tInt `fn` TAp tRatio (TGen 0))),
     "doubleToRatio" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (tDouble `fn` TAp tRatio (TGen 0))),
     "approxRational" :>:
       Forall [Star]
	 ([isIn1 cRealFrac (TGen 0)] :=>
	    (TGen 0 `fn` TGen 0 `fn` TAp tRatio tInteger)),
     "head" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "last" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "tail" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "init" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "null" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` tBool)),
     "++" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "filter" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "concat" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TAp tList (TGen 0)) `fn` TAp tList (TGen 0))),
     "foldl'" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TGen 0)),
     "length" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` tInt)),
     "!!" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` tInt `fn` TGen 0)),
     "foldl" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TGen 0)),
     "foldl1" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0)),
     "scanl" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 0))),
     "scanl1" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "foldr1" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0)),
     "scanr" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1))),
     "scanr1" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "repeat" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0 `fn` TAp tList (TGen 0))),
     "take" :>:
       Forall [Star]
	 ([] :=>
	    (tInt `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "replicate" :>:
       Forall [Star]
	 ([] :=>
	    (tInt `fn` TGen 0 `fn` TAp tList (TGen 0))),
     "cycle" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "drop" :>:
       Forall [Star]
	 ([] :=>
	    (tInt `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "splitAt" :>:
       Forall [Star]
	 ([] :=>
	    (tInt `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0)))),
     "dropWhile" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "span" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0)))),
     "break" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0)))),
     "lines" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp tList tChar))),
     "words" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp tList tChar))),
     "concatMap" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TAp tList (TGen 1)) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1))),
     "unlines" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp tList tChar) `fn` TAp tList tChar)),
     "unwords" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp tList tChar) `fn` TAp tList tChar)),
     "reverse" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "and" :>:
       Forall []
	 ([] :=>
	    (TAp tList tBool `fn` tBool)),
     "or" :>:
       Forall []
	 ([] :=>
	    (TAp tList tBool `fn` tBool)),
     "any" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` tBool)),
     "all" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` tBool)),
     "elem" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` tBool)),
     "notElem" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` tBool)),
     "lookup" :>:
       Forall [Star, Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp tMaybe (TGen 1))),
     "sum" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "product" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "maximum" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "minimum" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "zipWith" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2))),
     "zip" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1)))),
     "zipWith3" :>:
       Forall [Star, Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3))),
     "zip3" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)))),
     "unzip" :>:
       Forall [Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 (TGen 0)) (TGen 1)) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 1)))),
     "unzip3" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)) `fn` TAp (TAp (TAp tTuple3 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2)))),
     "reads" :>:
       Forall [Star]
	 ([isIn1 cRead (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "shows" :>:
       Forall [Star]
	 ([isIn1 cShow (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList tChar `fn` TAp tList tChar)),
     "nonnull" :>:
       Forall []
	 ([] :=>
	    ((tChar `fn` tBool) `fn` TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList tChar)))),
     "lexDigits" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList tChar)))),
     "lexmatch" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0)))),
     "asciiTab" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 tChar) (TAp tList tChar)))),
     "lexLitChar" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList tChar)))),
     "lex" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList tChar)))),
     "read" :>:
       Forall [Star]
	 ([isIn1 cRead (TGen 0)] :=>
	    (TAp tList tChar `fn` TGen 0)),
     "showChar" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` TAp tList tChar `fn` TAp tList tChar)),
     "showString" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList tChar `fn` TAp tList tChar)),
     "showParen" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` (TAp tList tChar `fn` TAp tList tChar) `fn` TAp tList tChar `fn` TAp tList tChar)),
     "showField" :>:
       Forall [Star]
	 ([isIn1 cShow (TGen 0)] :=>
	    (TAp tList tChar `fn` TGen 0 `fn` TAp tList tChar `fn` TAp tList tChar)),
     "readParen" :>:
       Forall [Star]
	 ([] :=>
	    (tBool `fn` (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar))) `fn` TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "readField" :>:
       Forall [Star]
	 ([isIn1 cRead (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "isOctDigit" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "isHexDigit" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` tBool)),
     "readInt" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` (tChar `fn` tBool) `fn` (tChar `fn` tInt) `fn` TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "readHex" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "readOct" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "readDec" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "readLitChar" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 tChar) (TAp tList tChar)))),
     "protectEsc" :>:
       Forall [Star]
	 ([] :=>
	    ((tChar `fn` tBool) `fn` (TAp tList tChar `fn` TGen 0) `fn` TAp tList tChar `fn` TGen 0)),
     "showLitChar" :>:
       Forall []
	 ([] :=>
	    (tChar `fn` TAp tList tChar `fn` TAp tList tChar)),
     "showInt" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList tChar `fn` TAp tList tChar)),
     "readSigned" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    ((TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar))) `fn` TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "showSigned" :>:
       Forall [Star]
	 ([isIn1 cReal (TGen 0)] :=>
	    ((TGen 0 `fn` TAp tList tChar `fn` TAp tList tChar) `fn` tInt `fn` TGen 0 `fn` TAp tList tChar `fn` TAp tList tChar)),
     "readFloat" :>:
       Forall [Star]
	 ([isIn1 cRealFloat (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList (TAp (TAp tTuple2 (TGen 0)) (TAp tList tChar)))),
     "putStrLn" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tIO tUnit)),
     "print" :>:
       Forall [Star]
	 ([isIn1 cShow (TGen 0)] :=>
	    (TGen 0 `fn` TAp tIO tUnit)),
     "getLine" :>:
       Forall []
	 ([] :=>
	    (TAp tIO (TAp tList tChar))),
     "readIO" :>:
       Forall [Star]
	 ([isIn1 cRead (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tIO (TGen 0))),
     "readLn" :>:
       Forall [Star]
	 ([isIn1 cRead (TGen 0)] :=>
	    (TAp tIO (TGen 0))),
     "interact" :>:
       Forall []
	 ([] :=>
	    ((TAp tList tChar `fn` TAp tList tChar) `fn` TAp tIO tUnit))]
