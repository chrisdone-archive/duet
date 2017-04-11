-----------------------------------------------------------------------------
-- Pat:		Patterns
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

module Pat where
import Id
import Kind
import Type
import Pred
import Scheme
import Assump
import TIMonad
import Infer
import Lit

data Pat        = PVar Id
                | PWildcard
                | PAs  Id Pat
                | PLit Literal
                | PNpk Id Integer
                | PCon Assump [Pat]

                | PLazy Pat

tiPat :: Pat -> TI ([Pred], [Assump], Type)

tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)

tiPat PWildcard   = do v <- newTVar Star
                       return ([], [], v)

tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, (i:>:toScheme t):as, t)

tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, [], t)

tiPat (PNpk i k)  = do t <- newTVar Star
                       return ([IsIn "Integral" [t]], [i:>:toScheme t], t)

tiPat (PCon (i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
                                t'         <- newTVar Star
                                (qs :=> t) <- freshInst sc
                                unify t (foldr fn t' ts)
                                return (ps++qs, as, t')

tiPat (PLazy pat) = tiPat pat

tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ ps' | (ps',_,_) <- psasts ]
                     as = concat [ as' | (_,as',_) <- psasts ]
                     ts = [ t | (_,_,t) <- psasts ]
                 return (ps, as, ts)

-----------------------------------------------------------------------------
