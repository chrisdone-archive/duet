-----------------------------------------------------------------------------
-- HaskellPrims:   Typing assumptions for primitives in the Hugs prelude
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

module HaskellPrims where
import Testbed
import StaticPrelude

defnsHaskellPrims
 = [ "_concmp" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` TGen 0 `fn` tOrdering))),
     "_range" :>: 
       (Forall [Star]
         ([] :=> 
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TAp tList (TGen 0)))),
     "_index" :>: 
       (Forall [Star]
         ([] :=> 
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TGen 0 `fn` tInt))),
     "_inRange" :>: 
       (Forall [Star]
         ([] :=> 
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TGen 0 `fn` tBool))),
     "_ToEnum" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` tInt `fn` TGen 0))),
     "_FrEnum" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` tInt))),
     "_From" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` TAp tList (TGen 0)))),
     "_FromTo" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     "_FromThen" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     "error" :>: 
       (Forall [Star]
         ([] :=> 
          (tString `fn` TGen 0))),
     "primIntToFloat" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tFloat))),
     "primIntToInteger" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInteger))),
     "ioeGetErrorString" :>: 
       (Forall []
         ([] :=> 
          (tIOError `fn` tString))),
     "readFile" :>: 
       (Forall []
         ([] :=> 
          (tFilePath `fn` TAp tIO tString))),
     "appendFile" :>: 
       (Forall []
         ([] :=> 
          (tFilePath `fn` tString `fn` TAp tIO tUnit))),
     "writeFile" :>: 
       (Forall []
         ([] :=> 
          (tFilePath `fn` tString `fn` TAp tIO tUnit))),
     "getContents" :>: 
       (Forall []
         ([] :=> 
          (TAp tIO tString))),
     "userError" :>: 
       (Forall []
         ([] :=> 
          (tString `fn` tIOError))),
     "getChar" :>: 
       (Forall []
         ([] :=> 
          (TAp tIO tChar))),
     "putStr" :>: 
       (Forall []
         ([] :=> 
          (tString `fn` TAp tIO tUnit))),
     "putChar" :>: 
       (Forall []
         ([] :=> 
          (tChar `fn` TAp tIO tUnit))),
     "ioError" :>: 
       (Forall [Star]
         ([] :=> 
          (tIOError `fn` TAp tIO (TGen 0)))),
     "catch" :>: 
       (Forall [Star]
         ([] :=> 
          (TAp tIO (TGen 0) `fn` (tIOError `fn` TAp tIO (TGen 0)) `fn` TAp tIO (TGen 0)))),
     "primretIO" :>: 
       (Forall [Star]
         ([] :=> 
          (TGen 0 `fn` TAp tIO (TGen 0)))),
     "primbindIO" :>: 
       (Forall [Star, Star]
         ([] :=> 
          (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1)))),
     "primShowsDouble" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tDouble `fn` tShowS))),
     "primShowsFloat" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tFloat `fn` tShowS))),
     "primDoubleDecode" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` TAp (TAp tTuple2 tInteger) tInt))),
     "primDoubleEncode" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInt `fn` tDouble))),
     "primDoubleMaxExp" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primDoubleMinExp" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primDoubleDigits" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primDoubleRadix" :>: 
       (Forall []
         ([] :=> 
          tInteger)),
     "primFloatDecode" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` TAp (TAp tTuple2 tInteger) tInt))),
     "primFloatEncode" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInt `fn` tFloat))),
     "primFloatMaxExp" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primFloatMinExp" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primFloatDigits" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primFloatRadix" :>: 
       (Forall []
         ([] :=> 
          tInteger)),
     "primSqrtDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primExpDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primLogDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primAtanDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primTanDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primAcosDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primCosDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primAsinDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primSinDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primSqrtFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primExpFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primLogFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primAtanFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primTanFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primAcosFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primCosFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primAsinFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primSinFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primRationalToDouble" :>: 
       (Forall []
         ([] :=> 
          (tRational `fn` tDouble))),
     "primRationalToFloat" :>: 
       (Forall []
         ([] :=> 
          (tRational `fn` tFloat))),
     "primDivDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tDouble))),
     "doubleToFloat" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tFloat))),
     "primDivFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tFloat))),
     "primIntegerToDouble" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tDouble))),
     "primIntToDouble" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tDouble))),
     "primNegDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble))),
     "primMulDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tDouble))),
     "primMinusDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tDouble))),
     "primPlusDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tDouble))),
     "primIntegerToFloat" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tFloat))),
     "primNegFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat))),
     "primMulFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tFloat))),
     "primMinusFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tFloat))),
     "primPlusFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tFloat))),
     "primCmpDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tOrdering))),
     "primEqDouble" :>: 
       (Forall []
         ([] :=> 
          (tDouble `fn` tDouble `fn` tBool))),
     "primCmpFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tOrdering))),
     "primEqFloat" :>: 
       (Forall []
         ([] :=> 
          (tFloat `fn` tFloat `fn` tBool))),
     "primShowsInteger" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInteger `fn` tShowS))),
     "primShowsInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tShowS))),
     "primEvenInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tBool))),
     "primQrmInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` TAp (TAp tTuple2 tInteger) tInteger))),
     "primEvenInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tBool))),
     "primQrmInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` TAp (TAp tTuple2 tInt) tInt))),
     "primModInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primRemInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primQuotInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primDivInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primNegInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger))),
     "primMulInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` tInteger))),
     "primMinusInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` tInteger))),
     "primPlusInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` tInteger))),
     "primMaxInt" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primMinInt" :>: 
       (Forall []
         ([] :=> 
          tInt)),
     "primIntegerToInt" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInt))),
     "primNegInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt))),
     "primMulInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primMinusInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primPlusInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tInt))),
     "primCmpInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` tOrdering))),
     "primEqInteger" :>: 
       (Forall []
         ([] :=> 
          (tInteger `fn` tInteger `fn` tBool))),
     "primCmpInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tOrdering))),
     "primEqInt" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tInt `fn` tBool))),
     "primIntToChar" :>: 
       (Forall []
         ([] :=> 
          (tInt `fn` tChar))),
     "primCharToInt" :>: 
       (Forall []
         ([] :=> 
          (tChar `fn` tInt))),
     "primCmpChar" :>: 
       (Forall []
         ([] :=> 
          (tChar `fn` tChar `fn` tOrdering))),
     "primEqChar" :>: 
       (Forall []
         ([] :=> 
          (tChar `fn` tChar `fn` tBool))),
     "$!" :>: 
       (Forall [Star, Star]
         ([] :=> 
          ((TGen 0 `fn` TGen 1) `fn` TGen 0 `fn` TGen 1))),
     "seq" :>: 
       (Forall [Star, Star]
         ([] :=> 
          (TGen 0 `fn` TGen 1 `fn` TGen 1))) ]

-----------------------------------------------------------------------------
