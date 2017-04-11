-- Automatically generated typing assumptions for Thih

module HaskellThih where
import Testbed
import StaticThih

defnsHaskellThih
 =  ["enumId" :>:
       Forall []
	 ([] :=>
	    (tInt `fn` TAp tList tChar)),
     "tUnit" :>:
       Forall []
	 ([] :=>
	    tType),
     "tChar" :>:
       Forall []
	 ([] :=>
	    tType),
     "tInt" :>:
       Forall []
	 ([] :=>
	    tType),
     "tInteger" :>:
       Forall []
	 ([] :=>
	    tType),
     "tFloat" :>:
       Forall []
	 ([] :=>
	    tType),
     "tDouble" :>:
       Forall []
	 ([] :=>
	    tType),
     "tList" :>:
       Forall []
	 ([] :=>
	    tType),
     "tArrow" :>:
       Forall []
	 ([] :=>
	    tType),
     "tTuple2" :>:
       Forall []
	 ([] :=>
	    tType),
     "fn" :>:
       Forall []
	 ([] :=>
	    (tType `fn` tType `fn` tType)),
     "list" :>:
       Forall []
	 ([] :=>
	    (tType `fn` tType)),
     "pair" :>:
       Forall []
	 ([] :=>
	    (tType `fn` tType `fn` tType)),
     "tString" :>:
       Forall []
	 ([] :=>
	    tType),
     "nullSubst" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 tTyvar) tType))),
     "+->" :>:
       Forall []
	 ([] :=>
	    (tTyvar `fn` tType `fn` TAp tList (TAp (TAp tTuple2 tTyvar) tType))),
     "@@" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 tTyvar) tType) `fn` TAp tList (TAp (TAp tTuple2 tTyvar) tType) `fn` TAp tList (TAp (TAp tTuple2 tTyvar) tType))),
     "merge" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp tList (TAp (TAp tTuple2 tTyvar) tType) `fn` TAp tList (TAp (TAp tTuple2 tTyvar) tType) `fn` TAp (TGen 0) (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "varBind" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tTyvar `fn` tType `fn` TAp (TGen 0) (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "mgu" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tType `fn` tType `fn` TAp (TGen 0) (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "match" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tType `fn` tType `fn` TAp (TGen 0) (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "lift" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((tType `fn` tType `fn` TAp (TGen 0) (TGen 1)) `fn` tPred `fn` tPred `fn` TAp (TGen 0) (TGen 1))),
     "mguPred" :>:
       Forall []
	 ([] :=>
	    (tPred `fn` tPred `fn` TAp tMaybe (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "matchPred" :>:
       Forall []
	 ([] :=>
	    (tPred `fn` tPred `fn` TAp tMaybe (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "super" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tChar `fn` TAp tList (TAp tList tChar))),
     "insts" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tChar `fn` TAp tList (TAp tQual tPred))),
     "defined" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tMaybe (TGen 0) `fn` tBool)),
     "modify" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tChar `fn` TAp (TAp tTuple2 (TAp tList (TAp tList tChar))) (TAp tList (TAp tQual tPred)) `fn` tClassEnv)),
     "initialEnv" :>:
       Forall []
	 ([] :=>
	    tClassEnv),
     "<:>" :>:
       Forall []
	 ([] :=>
	    ((tClassEnv `fn` TAp tMaybe tClassEnv) `fn` (tClassEnv `fn` TAp tMaybe tClassEnv) `fn` tClassEnv `fn` TAp tMaybe tClassEnv)),
     "addClass" :>:
       Forall []
	 ([] :=>
	    (TAp tList tChar `fn` TAp tList (TAp tList tChar) `fn` tClassEnv `fn` TAp tMaybe tClassEnv)),
     "addCoreClasses" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tMaybe tClassEnv)),
     "addNumClasses" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tMaybe tClassEnv)),
     "addPreludeClasses" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tMaybe tClassEnv)),
     "overlap" :>:
       Forall []
	 ([] :=>
	    (tPred `fn` tPred `fn` tBool)),
     "addInst" :>:
       Forall []
	 ([] :=>
	    (TAp tList tPred `fn` tPred `fn` tClassEnv `fn` TAp tMaybe tClassEnv)),
     "exampleInsts" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tMaybe tClassEnv)),
     "bySuper" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` tPred `fn` TAp tList tPred)),
     "byInst" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` tPred `fn` TAp tMaybe (TAp tList tPred))),
     "entail" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tPred `fn` tPred `fn` tBool)),
     "inHnf" :>:
       Forall []
	 ([] :=>
	    (tPred `fn` tBool)),
     "toHnf" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` tPred `fn` TAp (TGen 0) (TAp tList tPred))),
     "toHnfs" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred))),
     "simplify" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tPred `fn` TAp tList tPred)),
     "reduce" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred))),
     "scEntail" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tPred `fn` tPred `fn` tBool)),
     "quantify" :>:
       Forall []
	 ([] :=>
	    (TAp tList tTyvar `fn` TAp tQual tType `fn` tScheme)),
     "toScheme" :>:
       Forall []
	 ([] :=>
	    (tType `fn` tScheme)),
     "find" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (TAp tList tChar `fn` TAp tList tAssump `fn` TAp (TGen 0) tScheme)),
     "runTI" :>:
       Forall [Star]
	 ([] :=>
	    (TAp tTI (TGen 0) `fn` TGen 0)),
     "getSubst" :>:
       Forall []
	 ([] :=>
	    (TAp tTI (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "extSubst" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 tTyvar) tType) `fn` TAp tTI tUnit)),
     "unify" :>:
       Forall []
	 ([] :=>
	    (tType `fn` tType `fn` TAp tTI tUnit)),
     "newTVar" :>:
       Forall []
	 ([] :=>
	    (tKind `fn` TAp tTI tType)),
     "freshInst" :>:
       Forall []
	 ([] :=>
	    (tScheme `fn` TAp tTI (TAp tQual tType))),
     "tiLit" :>:
       Forall []
	 ([] :=>
	    (tLiteral `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) tType))),
     "tiPats" :>:
       Forall []
	 ([] :=>
	    (TAp tList tPat `fn` TAp tTI (TAp (TAp (TAp tTuple3 (TAp tList tPred)) (TAp tList tAssump)) (TAp tList tType)))),
     "tiPat" :>:
       Forall []
	 ([] :=>
	    (tPat `fn` TAp tTI (TAp (TAp (TAp tTuple3 (TAp tList tPred)) (TAp tList tAssump)) tType))),
     "numClasses" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp tList tChar))),
     "stdClasses" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp tList tChar))),
     "candidates" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp (TAp tTuple2 tTyvar) (TAp tList tPred) `fn` TAp tList tType)),
     "ambiguities" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp tList (TAp (TAp tTuple2 tTyvar) (TAp tList tPred)))),
     "withDefaults" :>:
       Forall [Kfun Star Star, Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    ((TAp tList (TAp (TAp tTuple2 tTyvar) (TAp tList tPred)) `fn` TAp tList tType `fn` TGen 1) `fn` tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TGen 1))),
     "defaultedPreds" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList tPred))),
     "split" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tPred)))),
     "tiSeq" :>:
       Forall [Star]
	 ([] :=>
	    ((tClassEnv `fn` TAp tList tAssump `fn` TGen 0 `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tAssump))) `fn` tClassEnv `fn` TAp tList tAssump `fn` TAp tList (TGen 0) `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tAssump)))),
     "restricted" :>:
       Forall []
	 ([] :=>
	    (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))) `fn` tBool)),
     "tiImpls" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))) `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tAssump)))),
     "tiAlt" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp (TAp tTuple2 (TAp tList tPat)) tExpr `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) tType))),
     "tiAlts" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr) `fn` tType `fn` TAp tTI (TAp tList tPred))),
     "tiExpl" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr)) `fn` TAp tTI (TAp tList tPred))),
     "tiBindGroup" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) (TAp tList tAssump)))),
     "tiExpr" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` tExpr `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) tType))),
     "defaultSubst" :>:
       Forall [Kfun Star Star]
	 ([isIn1 cMonad (TGen 0)] :=>
	    (tClassEnv `fn` TAp tList tTyvar `fn` TAp tList tPred `fn` TAp (TGen 0) (TAp tList (TAp (TAp tTuple2 tTyvar) tType)))),
     "tiProgram" :>:
       Forall []
	 ([] :=>
	    (tClassEnv `fn` TAp tList tAssump `fn` TAp tList (TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr)))))) `fn` TAp tList tAssump))]
