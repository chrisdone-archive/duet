-- Automatically generated typing assumptions for Maybe

module HaskellMaybe where
import Testbed
import StaticMaybe

defnsHaskellMaybe
 =  ["isJust" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tMaybe (TGen 0) `fn` tBool)),
     "isNothing" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tMaybe (TGen 0) `fn` tBool)),
     "fromJust" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tMaybe (TGen 0) `fn` TGen 0)),
     "fromMaybe" :>:
       Forall [Star]
	 ([] :=>
	    (TGen 0 `fn` TAp tMaybe (TGen 0) `fn` TGen 0)),
     "maybeToList" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tMaybe (TGen 0) `fn` TAp tList (TGen 0))),
     "listToMaybe" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TGen 0) `fn` TAp tMaybe (TGen 0))),
     "catMaybes" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tList (TAp tMaybe (TGen 0)) `fn` TAp tList (TGen 0))),
     "mapMaybe" :>:
       Forall [Star, Star]
	 ([] :=>
	    ((TGen 0 `fn` TAp tMaybe (TGen 1)) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))]
