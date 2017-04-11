-----------------------------------------------------------------------------
-- Type:		Types
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

module Type where 
import Id
import Kind
import PPrint

data Type  = TVar Tyvar | TCon Tycon | TAp  Type Type | TGen Int
             deriving Eq

data Tyvar = Tyvar Id Kind
             deriving Eq

data Tycon = Tycon Id Kind
             deriving Eq

instance PPrint Type where
  pprint    = pptype 0
  parPprint = pptype 10

pptype d (TAp (TAp a x) y)
    | a==tArrow    = ppParen (d>=5) (pptype 5 x <+> text "`fn`"
                                                <+> pptype 0 y)
pptype d (TAp l r) = ppParen (d>=10) (text "TAp" <+> pptype 10 l
                                                 <+> pptype 10 r)
pptype d (TGen n)  = ppParen (d>=10) (text "TGen" <+> int n)
pptype d t
    | t==tList     = text "tList"
    | t==tArrow    = text "tArrow"
    | t==tUnit     = text "tUnit"
    | t==tTuple2   = text "tTuple2"
    | t==tTuple3   = text "tTuple3"
    | t==tTuple4   = text "tTuple4"
    | t==tTuple5   = text "tTuple5"
    | t==tTuple6   = text "tTuple6"
    | t==tTuple7   = text "tTuple7"
pptype d (TCon (Tycon i k))
                   = text ('t':i)
pptype d (TVar v)  = pprint v

instance PPrint Tyvar where
  pprint (Tyvar v k)  = text v

tUnit    = TCon (Tycon "()" Star)
tChar    = TCon (Tycon "Char" Star)
tInt     = TCon (Tycon "Int" Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat   = TCon (Tycon "Float" Star)
tDouble  = TCon (Tycon "Double" Star)

tList    = TCon (Tycon "[]" (Kfun Star Star))
tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
tTuple2  = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tTuple3
 = TCon (Tycon "(,,)" (Kfun Star (Kfun Star (Kfun Star Star))))
tTuple4
 = TCon (Tycon "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))
tTuple5
 = TCon (Tycon "(,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))
tTuple6
 = TCon (Tycon "(,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))
tTuple7
 = TCon (Tycon "(,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))

tString    :: Type
tString     = list tChar

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list       :: Type -> Type
list t      = TAp tList t

pair       :: Type -> Type -> Type
pair a b    = TAp (TAp tTuple2 a) b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar v k) = k
instance HasKind Tycon where
  kind (Tycon v k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k

-----------------------------------------------------------------------------
