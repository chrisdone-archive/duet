-- Automatically generated typing assumptions for Monad

module HaskellMonad where
import Testbed
import StaticMonad

defnsHaskellMonad
 =  ["msum" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonadPlus (TGen 0)] :=>
	    (TAp tList (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TGen 1))),
     "join" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp (TGen 0) (TAp (TGen 0) (TGen 1)) `fn` TAp (TGen 0) (TGen 1))),
     "when" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tBool `fn` TAp (TGen 0) tUnit `fn` TAp (TGen 0) tUnit)),
     "unless" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tBool `fn` TAp (TGen 0) tUnit `fn` TAp (TGen 0) tUnit)),
     "liftM2" :>:
       Forall [Kfun Star Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TGen 3) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3))),
     "ap" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp (TGen 0) (TGen 1 `fn` TGen 2) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2))),
     "guard" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonadPlus (TGen 0)] :=>
	    (tBool `fn` TAp (TGen 0) tUnit)),
     "mapAndUnzipM" :>:
       Forall [Kfun Star Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TGen 2)) (TGen 3))) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TAp tList (TGen 2))) (TAp tList (TGen 3))))),
     "zipWithM" :>:
       Forall [Kfun Star Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 3)) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) (TAp tList (TGen 3)))),
     "zipWithM_" :>:
       Forall [Kfun Star Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 3)) `fn` TAp tList (TGen 1) `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) tUnit)),
     "foldM" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TAp (TGen 0) (TGen 1)) `fn` TGen 1 `fn` TAp tList (TGen 2) `fn` TAp (TGen 0) (TGen 1))),
     "filterM" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TAp (TGen 0) tBool) `fn` TAp tList (TGen 1) `fn` TAp (TGen 0) (TAp tList (TGen 1)))),
     "liftM" :>:
       Forall [Kfun Star Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2))),
     "liftM3" :>:
       Forall [Kfun Star Star, Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4))),
     "liftM4" :>:
       Forall [Kfun Star Star, Star, Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4) `fn` TAp (TGen 0) (TGen 5))),
     "liftM5" :>:
       Forall [Kfun Star Star, Star, Star, Star, Star, Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6) `fn` TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 3) `fn` TAp (TGen 0) (TGen 4) `fn` TAp (TGen 0) (TGen 5) `fn` TAp (TGen 0) (TGen 6)))]
