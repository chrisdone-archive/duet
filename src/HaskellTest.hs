-- Automatically generated typing assumptions for Test

module HaskellTest where
import Testbed
import StaticTest

defnsHaskellTest
 =  ["test" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "tail'" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "head'" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "aFloat" :>:
       Forall []
	 ([] :=>
	    tFloat),
     "aNum" :>:
       Forall []
	 ([] :=>
	    tFloat),
     "bNum" :>:
       Forall []
	 ([] :=>
	    tFloat),
     "cNum" :>:
       Forall []
	 ([] :=>
	    (tFloat `fn` tFloat)),
     "oddMono" :>:
       Forall []
	 ([] :=>
	    (tInteger `fn` tBool)),
     "sumMono" :>:
       Forall []
	 ([] :=>
	    (TAp tList tInteger `fn` tInteger)),
     "sumEta" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TGen 0)),
     "egg" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "egf" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "egf'" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "egg'" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "egg''" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` tBool)),
     "egf''" :>:
       Forall []
	 ([] :=>
	    (tBool `fn` tBool)),
     "myEven" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "myOdd" :>:
       Forall [Star]
	 ([isIn1 cNum (TGen 0)] :=>
	    (TGen 0 `fn` tBool)),
     "eqNull" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` tBool)),
     "eqNull'" :>:
       Forall [Star]
	 ([isIn1 cEq (TAp tList (TGen 0))] :=>
	    (TAp tList (TGen 0) `fn` tBool)),
     "baz" :>:
       Forall [Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TAp (TAp tTuple2 (TAp (TAp tTuple2 (TGen 0 `fn` tBool)) (TGen 0 `fn` tBool))) (TAp (TAp tTuple2 (TGen 0 `fn` TGen 0)) (TGen 0 `fn` TGen 0))))]
