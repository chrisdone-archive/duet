-- Automatically generated typing assumptions for List

module HaskellList where
import Testbed
import StaticList

defnsHaskellList
 =  ["findIndices" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList tInt)),
     "findIndex" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tMaybe tInt)),
     "elemIndex" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tMaybe tInt)),
     "elemIndices" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList tInt)),
     "find" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tMaybe (TGen 0))),
     "nubBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "nub" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "deleteBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "delete" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "\\\\" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "deleteFirstsBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "unionBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "union" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "intersectBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "intersect" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "intersperse" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "transpose" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TAp tList (TGen 0)) `fn` TAp tList (TAp tList (TGen 0)))),
     "partition" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp (TAp tTuple2 (TAp tList (TGen 0))) (TAp tList (TGen 0)))),
     "groupBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tBool) `fn` TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0)))),
     "group" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0)))),
     "inits" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0)))),
     "tails" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TAp tList (TGen 0)))),
     "isPrefixOf" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tBool)),
     "isSuffixOf" :>:
       Forall [Star]
	 ([isIn1 cEq (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0) `fn` tBool)),
     "mapAccumL" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 2)) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TAp tList (TGen 2)))),
     "mapAccumR" :>:
       Forall [Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 2)) `fn` TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TGen 0)) (TAp tList (TGen 2)))),
     "unfoldr" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TAp tMaybe (TAp (TAp tTuple2 (TGen 1)) (TGen 0))) `fn` TGen 0 `fn` TAp tList (TGen 1))),
     "insertBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tOrdering) `fn` TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "sortBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` tOrdering) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "sort" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "insert" :>:
       Forall [Star]
	 ([isIn1 cOrd (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))),
     "maximumBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0)),
     "minimumBy" :>:
       Forall [Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 0 `fn` TGen 0) `fn` TAp tList (TGen 0) `fn` TGen 0)),
     "genericLength" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tList (TGen 1) `fn` TGen 0)),
     "genericTake" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 1))),
     "genericDrop" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 1))),
     "genericSplitAt" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TAp tList (TGen 1) `fn` TAp (TAp tTuple2 (TAp tList (TGen 1))) (TAp tList (TGen 1)))),
     "genericIndex" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TAp tList (TGen 1) `fn` TGen 0 `fn` TGen 1)),
     "genericReplicate" :>:
       Forall [Star, Star]
	 ([isIn1 cIntegral (TGen 0)] :=>
	    (TGen 0 `fn` TGen 1 `fn` TAp tList (TGen 1))),
     "zipWith4" :>:
       Forall [Star, Star, Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4))),
     "zip4" :>:
       Forall [Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)))),
     "zipWith5" :>:
       Forall [Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5))),
     "zip5" :>:
       Forall [Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)))),
     "zipWith6" :>:
       Forall [Star, Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6))),
     "zip6" :>:
       Forall [Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)))),
     "zipWith7" :>:
       Forall [Star, Star, Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6 `fn` TGen 7) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6) `fn` TAp tList (TGen 7))),
     "zip7" :>:
       Forall [Star, Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp tList (TGen 3) `fn` TAp tList (TGen 4) `fn` TAp tList (TGen 5) `fn` TAp tList (TGen 6) `fn` TAp tList (TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) (TGen 6)))),
     "unzip4" :>:
       Forall [Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) `fn` TAp (TAp (TAp (TAp tTuple4 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3)))),
     "unzip5" :>:
       Forall [Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4)))),
     "unzip6" :>:
       Forall [Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) `fn` TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4))) (TAp tList (TGen 5)))),
     "unzip7" :>:
       Forall [Star, Star, Star, Star, Star, Star, Star]
	 ([] :=>
	    (TAp tList (TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) (TGen 6)) `fn` TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TAp tList (TGen 0))) (TAp tList (TGen 1))) (TAp tList (TGen 2))) (TAp tList (TGen 3))) (TAp tList (TGen 4))) (TAp tList (TGen 5))) (TAp tList (TGen 6))))]
