-----------------------------------------------------------------------------
-- TIMain:	Type Inference Algorithm
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

module TIMain where
import Data.List( (\\), intersect, union, partition )

import Id
import Kind
import Type
import Subst
import Pred
import Scheme
import Assump
import TIMonad
import Infer
import Lit
import Pat
import StaticPrelude

-----------------------------------------------------------------------------

data Expr = Var   Id
          | Lit   Literal
          | Const Assump
          | Ap    Expr Expr
          | Let   BindGroup Expr

          | Lam   Alt
          | If    Expr Expr Expr
          | Case  Expr [(Pat,Expr)]

-----------------------------------------------------------------------------
-- The following helper functions are used to construct sample programs; if we
-- change the representation of Expr above, then we need only change the
-- definitions of the following combinators to match, and do not need to
-- rewrite all the test code.

ap              = foldl1 Ap
evar v          = (Var v)
elit l          = (Lit l)
econst c        = (Const c)
elet e f        = foldr Let f (map toBg e)

toBg           :: [(Id, Maybe Scheme, [Alt])] -> BindGroup
toBg g          = ([(v, t, alts) | (v, Just t, alts) <- g ],
                   filter (not . null) [[(v,alts) | (v,Nothing,alts) <- g]])

pNil            = PCon nilCfun []
pCons x y       = PCon consCfun [x,y]

eNil            = econst nilCfun
eCons x y       = ap [ econst consCfun, x, y ]

{-
ecase           = Case
elambda         = Lam
eif             = If
-}

ecase d as      = elet [[ ("_case",
                           Nothing,
                           [([p],e) | (p,e) <- as]) ]]
                       (ap [evar "_case", d])
eif c t f       = ecase c [(PCon trueCfun [], t),(PCon falseCfun [], f)]
elambda alt     = elet [[ ("_lambda",
                           Nothing,
                           [alt]) ]]
                             (evar "_lambda")
eguarded        = foldr (\(c,t) e -> eif c t e) efail
efail           = Const ("FAIL" :>: Forall [Star] ([] :=> TGen 0))
esign e t       = elet [[ ("_val", Just t, [([],e)]) ]] (evar "_val")

eCompFrom p e c = ap [ econst mbindMfun, e, elambda ([p],c) ]
eCompGuard e c  = eif e c eNil
eCompLet bgs c  = elet bgs c
eListRet e      = eCons e eNil

-----------------------------------------------------------------------------

tiExpr                       :: Infer Expr Type
tiExpr ce as (Var i)          = do sc         <- find i as
                                   (ps :=> t) <- freshInst sc
                                   return (ps, t)
tiExpr ce as (Const (i:>:sc)) = do (ps :=> t) <- freshInst sc
                                   return (ps, t)
tiExpr ce as (Lit l)          = do (ps,t) <- tiLit l
                                   return (ps, t)
tiExpr ce as (Ap e f)         = do (ps,te) <- tiExpr ce as e
                                   (qs,tf) <- tiExpr ce as f
                                   t       <- newTVar Star
                                   unify (tf `fn` t) te
                                   return (ps++qs, t)
tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
                                   (qs, t)   <- tiExpr ce (as' ++ as) e
                                   return (ps ++ qs, t)

tiExpr ce as (Lam alt)
  = tiAlt ce as alt
tiExpr ce as (If e e1 e2)
  = do (ps,t)   <- tiExpr ce as e
       unify t tBool
       (ps1,t1) <- tiExpr ce as e1
       (ps2,t2) <- tiExpr ce as e2
       unify t1 t2
       return (ps++ps1++ps2, t1)
tiExpr ce as (Case e branches)
  = do (ps, t) <- tiExpr ce as e
       v       <- newTVar Star
       let tiBr (pat, f)
            = do (ps, as',t') <- tiPat pat
                 unify t t'
                 (qs, t'')   <- tiExpr ce (as'++as) f
                 unify v t''
                 return (ps++qs)
       pss <- mapM tiBr branches
       return (ps++concat pss, v)

-----------------------------------------------------------------------------

type Alt = ([Pat], Expr)

tiAlt                :: Infer Alt Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
                           (qs,t)  <- tiExpr ce (as'++as) e
                           return (ps++qs, foldr fn t ts)

tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM (unify t) (map snd psts)
                         return (concat (map fst psts))

-----------------------------------------------------------------------------

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
                      -> m ([Pred], [Pred])
split ce fs gs ps = do let ps' = reduce ce ps
                           (ds, rs) = partition (all (`elem` fs) . tv) ps'
                       rs' <- defaultedPreds ce (fs++gs) rs
                       return (ds, rs \\ rs')

type Ambiguity       = (Tyvar, [Pred])

ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [ (v, filter (elem v . tv) ps) | v <- tv ps \\ vs ]

numClasses :: [Id]
numClasses  = ["Num", "Integral", "Floating", "Fractional",
               "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
               "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates           :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
                                   ts = [ t | IsIn i t <- qs ],
                               all ([TVar v]==) ts,
                               any (`elem` numClasses) is,
                               all (`elem` stdClasses) is,
                               t' <- defaults ce,
                               all (entail ce []) [ IsIn i [t'] | i <- is ] ]

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
                  -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
    | any null tss  = fail "cannot resolve ambiguity"
    | otherwise     = return (f vps (map head tss))
      where vps = ambiguities ce vs ps
            tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps ts -> concat (map snd vps))

defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)

-----------------------------------------------------------------------------

type Expl = (Id, Scheme, [Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i, sc, alts)
        = do (qs :=> t) <- freshInst sc
             ps         <- tiAlts ce as alts t
             s          <- getSubst
             let qs'     = apply s qs
                 t'      = apply s t
                 fs      = tv (apply s as)
                 gs      = tv t' \\ fs
                 sc'     = quantify gs (qs':=>t')
                 ps'     = filter (not . entail ce qs') (apply s ps)
             (ds,rs)    <- split ce fs gs ps'
             if sc /= sc' then
                 fail "signature too general"
               else if not (null rs) then
                 fail "context too weak"
               else
                 return ds

-----------------------------------------------------------------------------

type Impl   = (Id, [Alt])

restricted   :: [Impl] -> Bool
restricted bs = any simple bs
 where simple (i,alts) = any (null . fst) alts

tiImpls         :: Infer [Impl] [Assump]
tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                      let is    = map fst bs
                          scs   = map toScheme ts
                          as'   = zipWith (:>:) is scs ++ as
                          altss = map snd bs
                      pss <- sequence (zipWith (tiAlts ce as') altss ts)
                      s   <- getSubst
                      let ps'     = apply s (concat pss)
                          ts'     = apply s ts
                          fs      = tv (apply s as)
                          vss     = map tv ts'
                          gs      = foldr1 union vss \\ fs
                      (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
                      if restricted bs then
                          let gs'  = gs \\ tv rs
                              scs' = map (quantify gs' . ([]:=>)) ts'
                          in return (ds++rs, zipWith (:>:) is scs')
                        else
                          let scs' = map (quantify gs . (rs:=>)) ts'
                          in return (ds, zipWith (:>:) is scs')

-----------------------------------------------------------------------------

type BindGroup  = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es,iss) =
  do let as' = [ v:>:sc | (v,sc,alts) <- es ]
     (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
     qss        <- mapM (tiExpl ce (as''++as'++as)) es
     return (ps++concat qss, as''++as')

tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as []       = return ([],[])
tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
                             (qs,as'') <- tiSeq ti ce (as'++as) bss
                             return (ps++qs, as''++as')

-----------------------------------------------------------------------------
