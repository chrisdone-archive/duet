-----------------------------------------------------------------------------
-- HaskellPrims:   Typing assumptiontions for primitives in the Hugs prelude
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

defnsHaskellPrims :: [Assumption]
defnsHaskellPrims
 = [ Assumption "_concmp"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` TGen 0 `fn` tOrdering))),
     Assumption "_range"
       (Forall [Star]
         ([] :=>
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TAp tList (TGen 0)))),
     Assumption "_index"
       (Forall [Star]
         ([] :=>
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TGen 0 `fn` tInt))),
     Assumption "_inRange"
       (Forall [Star]
         ([] :=>
          (TAp (TAp tTuple2 (TGen 0)) (TGen 0) `fn` TGen 0 `fn` tBool))),
     Assumption "_ToEnum"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` tInt `fn` TGen 0))),
     Assumption "_FrEnum"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` tInt))),
     Assumption "_From"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` TAp tList (TGen 0)))),
     Assumption "_FromTo"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     Assumption "_FromThen"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` TGen 0 `fn` TAp tList (TGen 0)))),
     Assumption "error"
       (Forall [Star]
         ([] :=>
          (tString `fn` TGen 0))),
     Assumption "primIntToFloat"
       (Forall []
         ([] :=>
          (tInt `fn` tFloat))),
     Assumption "primIntToInteger"
       (Forall []
         ([] :=>
          (tInt `fn` tInteger))),
     Assumption "ioeGetErrorString"
       (Forall []
         ([] :=>
          (tIOError `fn` tString))),
     Assumption "readFile"
       (Forall []
         ([] :=>
          (tFilePath `fn` TAp tIO tString))),
     Assumption "appendFile"
       (Forall []
         ([] :=>
          (tFilePath `fn` tString `fn` TAp tIO tUnit))),
     Assumption "writeFile"
       (Forall []
         ([] :=>
          (tFilePath `fn` tString `fn` TAp tIO tUnit))),
     Assumption "getContents"
       (Forall []
         ([] :=>
          (TAp tIO tString))),
     Assumption "userError"
       (Forall []
         ([] :=>
          (tString `fn` tIOError))),
     Assumption "getChar"
       (Forall []
         ([] :=>
          (TAp tIO tChar))),
     Assumption "putStr"
       (Forall []
         ([] :=>
          (tString `fn` TAp tIO tUnit))),
     Assumption "putChar"
       (Forall []
         ([] :=>
          (tChar `fn` TAp tIO tUnit))),
     Assumption "ioError"
       (Forall [Star]
         ([] :=>
          (tIOError `fn` TAp tIO (TGen 0)))),
     Assumption "catch"
       (Forall [Star]
         ([] :=>
          (TAp tIO (TGen 0) `fn` (tIOError `fn` TAp tIO (TGen 0)) `fn` TAp tIO (TGen 0)))),
     Assumption "primretIO"
       (Forall [Star]
         ([] :=>
          (TGen 0 `fn` TAp tIO (TGen 0)))),
     Assumption "primbindIO"
       (Forall [Star, Star]
         ([] :=>
          (TAp tIO (TGen 0) `fn` (TGen 0 `fn` TAp tIO (TGen 1)) `fn` TAp tIO (TGen 1)))),
     Assumption "primShowsDouble"
       (Forall []
         ([] :=>
          (tInt `fn` tDouble `fn` tShowS))),
     Assumption "primShowsFloat"
       (Forall []
         ([] :=>
          (tInt `fn` tFloat `fn` tShowS))),
     Assumption "primDoubleDecode"
       (Forall []
         ([] :=>
          (tDouble `fn` TAp (TAp tTuple2 tInteger) tInt))),
     Assumption "primDoubleEncode"
       (Forall []
         ([] :=>
          (tInteger `fn` tInt `fn` tDouble))),
     Assumption "primDoubleMaxExp"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primDoubleMinExp"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primDoubleDigits"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primDoubleRadix"
       (Forall []
         ([] :=>
          tInteger)),
     Assumption "primFloatDecode"
       (Forall []
         ([] :=>
          (tFloat `fn` TAp (TAp tTuple2 tInteger) tInt))),
     Assumption "primFloatEncode"
       (Forall []
         ([] :=>
          (tInteger `fn` tInt `fn` tFloat))),
     Assumption "primFloatMaxExp"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primFloatMinExp"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primFloatDigits"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primFloatRadix"
       (Forall []
         ([] :=>
          tInteger)),
     Assumption "primSqrtDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primExpDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primLogDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primAtanDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primTanDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primAcosDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primCosDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primAsinDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primSinDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primSqrtFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primExpFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primLogFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primAtanFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primTanFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primAcosFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primCosFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primAsinFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primSinFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primRationalToDouble"
       (Forall []
         ([] :=>
          (tRational `fn` tDouble))),
     Assumption "primRationalToFloat"
       (Forall []
         ([] :=>
          (tRational `fn` tFloat))),
     Assumption "primDivDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tDouble))),
     Assumption "doubleToFloat"
       (Forall []
         ([] :=>
          (tDouble `fn` tFloat))),
     Assumption "primDivFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tFloat))),
     Assumption "primIntegerToDouble"
       (Forall []
         ([] :=>
          (tInteger `fn` tDouble))),
     Assumption "primIntToDouble"
       (Forall []
         ([] :=>
          (tInt `fn` tDouble))),
     Assumption "primNegDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble))),
     Assumption "primMulDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tDouble))),
     Assumption "primMinusDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tDouble))),
     Assumption "primPlusDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tDouble))),
     Assumption "primIntegerToFloat"
       (Forall []
         ([] :=>
          (tInteger `fn` tFloat))),
     Assumption "primNegFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat))),
     Assumption "primMulFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tFloat))),
     Assumption "primMinusFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tFloat))),
     Assumption "primPlusFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tFloat))),
     Assumption "primCmpDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tOrdering))),
     Assumption "primEqDouble"
       (Forall []
         ([] :=>
          (tDouble `fn` tDouble `fn` tBool))),
     Assumption "primCmpFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tOrdering))),
     Assumption "primEqFloat"
       (Forall []
         ([] :=>
          (tFloat `fn` tFloat `fn` tBool))),
     Assumption "primShowsInteger"
       (Forall []
         ([] :=>
          (tInt `fn` tInteger `fn` tShowS))),
     Assumption "primShowsInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tShowS))),
     Assumption "primEvenInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tBool))),
     Assumption "primQrmInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` TAp (TAp tTuple2 tInteger) tInteger))),
     Assumption "primEvenInt"
       (Forall []
         ([] :=>
          (tInt `fn` tBool))),
     Assumption "primQrmInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` TAp (TAp tTuple2 tInt) tInt))),
     Assumption "primModInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primRemInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primQuotInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primDivInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primNegInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger))),
     Assumption "primMulInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` tInteger))),
     Assumption "primMinusInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` tInteger))),
     Assumption "primPlusInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` tInteger))),
     Assumption "primMaxInt"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primMinInt"
       (Forall []
         ([] :=>
          tInt)),
     Assumption "primIntegerToInt"
       (Forall []
         ([] :=>
          (tInteger `fn` tInt))),
     Assumption "primNegInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt))),
     Assumption "primMulInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primMinusInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primPlusInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tInt))),
     Assumption "primCmpInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` tOrdering))),
     Assumption "primEqInteger"
       (Forall []
         ([] :=>
          (tInteger `fn` tInteger `fn` tBool))),
     Assumption "primCmpInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tOrdering))),
     Assumption "primEqInt"
       (Forall []
         ([] :=>
          (tInt `fn` tInt `fn` tBool))),
     Assumption "primIntToChar"
       (Forall []
         ([] :=>
          (tInt `fn` tChar))),
     Assumption "primCharToInt"
       (Forall []
         ([] :=>
          (tChar `fn` tInt))),
     Assumption "primCmpChar"
       (Forall []
         ([] :=>
          (tChar `fn` tChar `fn` tOrdering))),
     Assumption "primEqChar"
       (Forall []
         ([] :=>
          (tChar `fn` tChar `fn` tBool))),
     Assumption "$!"
       (Forall [Star, Star]
         ([] :=>
          ((TGen 0 `fn` TGen 1) `fn` TGen 0 `fn` TGen 1))),
     Assumption "seq"
       (Forall [Star, Star]
         ([] :=>
          (TGen 0 `fn` TGen 1 `fn` TGen 1))) ]

-----------------------------------------------------------------------------
