-----------------------------------------------------------------------------
-- StaticThih:          Static environment for Thih
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

module StaticThih(module StaticPrelude,
                  module StaticList,
                  module StaticMonad,
                  module StaticThih) where
import Static
import StaticPrelude
import StaticList
import StaticMonad

-----------------------------------------------------------------------------
-- Thih types:

tId :: Type
tId = TAp tList tChar

tKind :: Type
tKind = TCon (Tycon "Kind" Star)
starCfun :: Assump
starCfun = "Star" :>: (Forall [] ([] :=> tKind))
kfunCfun :: Assump
kfunCfun = "Kfun" :>: (Forall [] ([] :=> (tKind `fn` tKind `fn` tKind)))

tType :: Type
tType = TCon (Tycon "Type" Star)
tVarCfun :: Assump
tVarCfun = "TVar" :>: (Forall [] ([] :=> (tTyvar `fn` tType)))
tConCfun :: Assump
tConCfun = "TCon" :>: (Forall [] ([] :=> (tTycon `fn` tType)))
tApCfun :: Assump
tApCfun = "TAp" :>: (Forall [] ([] :=> (tType `fn` tType `fn` tType)))
tGenCfun :: Assump
tGenCfun = "TGen" :>: (Forall [] ([] :=> (tInt `fn` tType)))

tTyvar :: Type
tTyvar = TCon (Tycon "Tyvar" Star)
tyvarCfun :: Assump
tyvarCfun = "Tyvar" :>: (Forall [] ([] :=> (tId `fn` tKind `fn` tTyvar)))

tTycon :: Type
tTycon = TCon (Tycon "Tycon" Star)
tyconCfun :: Assump
tyconCfun = "Tycon" :>: (Forall [] ([] :=> (tId `fn` tKind `fn` tTycon)))

tSubst :: Type
tSubst = TAp tList (TAp (TAp tTuple2 tTyvar) tType)

tQual :: Type
tQual = TCon (Tycon "Qual" (Kfun Star Star))
qualifyCfun :: Assump
qualifyCfun
 = ":=>" :>: (Forall [Star]
               ([] :=>
                (TAp tList tPred `fn` TGen 0 `fn` TAp tQual (TGen 0))))

tPred :: Type
tPred = TCon (Tycon "Pred" Star)
isInCfun :: Assump
isInCfun = "IsIn" :>: (Forall [] ([] :=> (tId `fn` tType `fn` tPred)))

tClass :: Type
tClass
 = TAp (TAp tTuple2 (TAp tList (TAp tList tChar))) (TAp tList (TAp tQual tPred))

tInst :: Type
tInst = TAp tQual tPred

tClassEnv :: Type
tClassEnv = TCon (Tycon "ClassEnv" Star)
classEnvCfun :: Assump
classEnvCfun
 = "ClassEnv" :>: (Forall []
                    ([] :=>
                     ((tId `fn` TAp tMaybe tClass) `fn` TAp tList tType `fn` tClassEnv)))
classesSfun :: Assump
classesSfun
 = "classes" :>: (Forall []
                   ([] :=>
                    (tClassEnv `fn` tId `fn` TAp tMaybe tClass)))
defaultsSfun :: Assump
defaultsSfun
 = "defaults" :>: (Forall []
                    ([] :=>
                     (tClassEnv `fn` TAp tList tType)))

tEnvTransformer :: Type
tEnvTransformer
 = tClassEnv `fn` TAp tMaybe tClassEnv

tScheme :: Type
tScheme
 = TCon (Tycon "Scheme" Star)
forallCfun :: Assump
forallCfun
 = "Forall" :>: (Forall []
                  ([] :=>
                   (TAp tList tKind `fn` TAp tQual tType `fn` tScheme)))

tAssump :: Type
tAssump
 = TCon (Tycon "Assump" Star)
assumeCfun :: Assump
assumeCfun
 = ":>:" :>: (Forall []
               ([] :=>
                (tId `fn` tScheme `fn` tAssump)))

tTI :: Type
tTI
 = TCon (Tycon "TI" (Kfun Star Star))
tICfun :: Assump
tICfun
 = "TI" :>: (Forall [Star]
              ([] :=>
               ((tSubst `fn` tInt `fn` TAp (TAp (TAp tTuple3 tSubst) tInt) (TGen 0)) `fn` TAp tTI (TGen 0))))

tInfer :: Type -> Type -> Type
tInfer a b
 = tClassEnv `fn` TAp tList tAssump `fn` a `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) b)

tLiteral :: Type
tLiteral = TCon (Tycon "Literal" Star)
litIntCfun :: Assump
litIntCfun = "LitInt" :>: (Forall [] ([] :=> (tInteger `fn` tLiteral)))
litCharCfun :: Assump
litCharCfun = "LitChar" :>: (Forall [] ([] :=> (tChar `fn` tLiteral)))
litRatCfun :: Assump
litRatCfun = "LitRat" :>: (Forall [] ([] :=> (tRational `fn` tLiteral)))
litStrCfun :: Assump
litStrCfun = "LitStr" :>: (Forall [] ([] :=> (tString `fn` tLiteral)))

tPat :: Type
tPat = TCon (Tycon "Pat" Star)
pVarCfun :: Assump
pVarCfun = "PVar" :>: (Forall [] ([] :=> (tId `fn` tPat)))
pWildcardCfun :: Assump
pWildcardCfun = "PWildcard" :>: (Forall [] ([] :=> tPat))
pAsCfun :: Assump
pAsCfun = "PAs" :>: (Forall [] ([] :=> (tId `fn` tPat `fn` tPat)))
pLitCfun :: Assump
pLitCfun = "PLit" :>: (Forall [] ([] :=> (tLiteral `fn` tPat)))
pNpkCfun :: Assump
pNpkCfun = "PNpk" :>: (Forall [] ([] :=> (tId `fn` tInteger `fn` tPat)))
pConCfun :: Assump
pConCfun = "PCon" :>: (Forall [] ([] :=> (tAssump `fn` TAp tList tPat `fn` tPat)))

tExpr :: Type
tExpr = TCon (Tycon "Expr" Star)
varCfun :: Assump
varCfun = "Var" :>: (Forall [] ([] :=> (tId `fn` tExpr)))
litCfun :: Assump
litCfun = "Lit" :>: (Forall [] ([] :=> (tLiteral `fn` tExpr)))
constCfun :: Assump
constCfun = "Const" :>: (Forall [] ([] :=> (tAssump `fn` tExpr)))
apCfun :: Assump
apCfun = "Ap" :>: (Forall [] ([] :=> (tExpr `fn` tExpr `fn` tExpr)))
letCfun :: Assump
letCfun = "Let" :>: (Forall [] ([] :=> (tBindGroup `fn` tExpr `fn` tExpr)))

tAlt :: Type
tAlt = TAp (TAp tTuple2 (TAp tList tPat)) tExpr

tAmbiguity :: Type
tAmbiguity = TAp (TAp tTuple2 tTyvar) (TAp tList tPred)

tExpl :: Type
tExpl
 = TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))

tImpl :: Type
tImpl
 = TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))

tBindGroup :: Type
tBindGroup
 = TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr)))))

tProgram :: Type
tProgram
 = TAp tList (TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))))

-----------------------------------------------------------------------------

thihClasses :: ClassEnv -> Maybe ClassEnv
thihClasses =   addClass cHasKind asig []
            <:> addClass cTypes asig []
            <:> addClass cInstantiate asig []
            <:> instances instsThih

instsThih :: [Qual Pred]
instsThih
 = [mkInst [] ([] :=> isIn1 cEq tKind),
    mkInst [] ([] :=> isIn1 cEq tType),
    mkInst [] ([] :=> isIn1 cEq tTyvar),
    mkInst [] ([] :=> isIn1 cEq tTycon),
    mkInst [Star] ([isIn1 cEq (TGen 0)] :=> isIn1 cEq (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cEq tPred),
    mkInst [] ([] :=> isIn1 cEq tScheme),
    mkInst [] ([] :=> isIn1 cMonad tTI),
    mkInst [] ([] :=> isIn1 cHasKind tTyvar),
    mkInst [] ([] :=> isIn1 cHasKind tTycon),
    mkInst [] ([] :=> isIn1 cHasKind tType),
    mkInst [] ([] :=> isIn1 cTypes tType),
    mkInst [Star] ([isIn1 cTypes (TGen 0)] :=>
      isIn1 cTypes (TAp tList (TGen 0))),
    mkInst [Star] ([isIn1 cTypes (TGen 0)] :=>
      isIn1 cTypes (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cTypes tPred),
    mkInst [] ([] :=> isIn1 cTypes tScheme),
    mkInst [] ([] :=> isIn1 cTypes tAssump),
    mkInst [] ([] :=> isIn1 cInstantiate tType),
    mkInst [Star] ([isIn1 cInstantiate (TGen 0)] :=>
      isIn1 cInstantiate (TAp tList (TGen 0))),
    mkInst [Star] ([isIn1 cInstantiate (TGen 0)] :=>
      isIn1 cInstantiate (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cInstantiate tPred)]

cHasKind :: String
cHasKind = "HasKind"
kindMfun :: Assump
kindMfun
 = "kind" :>: (Forall [Star]
                ([isIn1 cHasKind (TGen 0)] :=> (TGen 0 `fn` tKind)))

cTypes :: String
cTypes = "Types"

applyMfun :: Assump
applyMfun
 = "apply" :>: (Forall [Star]
                 ([isIn1 cTypes (TGen 0)] :=>
                  (tSubst `fn` TGen 0 `fn` TGen 0)))
tvMfun :: Assump
tvMfun
 = "tv" :>: (Forall [Star]
              ([isIn1 cTypes (TGen 0)] :=>
               (TGen 0 `fn` TAp tList tTyvar)))

cInstantiate :: String
cInstantiate = "Instantiate"

instMfun :: Assump
instMfun
 = "inst" :>: (Forall [Star]
                ([isIn1 cInstantiate (TGen 0)] :=>
                 (TAp tList tType `fn` TGen 0 `fn` TGen 0)))

-----------------------------------------------------------------------------
