{-# LANGUAGE NoMonomorphismRestriction #-}
module THIH where
import qualified Control.Monad
import Control.Monad hiding (ap)
import Data.List hiding (find)
import Debug.Trace
import Text.PrettyPrint.HughesPJ

-- | Assumptions about the type of a variable are represented by
-- values of the Assump datatype, each of which pairs a variable name
-- with a type scheme.
data Assumption = Assumption
  { assumptionId :: Id
  , assumptionScheme :: Scheme
  }

type Program = [BindGroup]
type BindGroup = ([Expl], [[Impl]])
type Impl = (Id, [Alt])
type Expl = (Id, Scheme, [Alt])
type Ambiguity = (Tyvar, [Pred])
type Alt = ([Pat], Expr)
type Subst = [(Tyvar, Type)]
type EnvTransformer = ClassEnv -> Maybe ClassEnv
type Inst = Qual Pred
type Class = ([Tyvar], [Pred], [Inst])
type Infer e t = ClassEnv -> [Assumption] -> e -> TI ([Pred], t)
type Id = String

data Tycon =
  Tycon Id Kind
  deriving (Eq)
data Tyvar =
  Tyvar Id
        Kind
  deriving (Eq)
data Type
  = TVar Tyvar
  | TCon Tycon
  | TAp Type Type
  | TGen Int
  deriving (Eq)
data Expr
  = Var Id
  | Lit Literal
  | Const Assumption
  | Ap Expr Expr
  | Let BindGroup Expr
  | Lam Alt
  | If Expr Expr Expr
  | Case Expr [(Pat, Expr)]
data Scheme =
  Forall [Kind] (Qual Type)
  deriving (Eq)
data ClassEnv = ClassEnv
  { classes :: Id -> Maybe Class
  , defaults :: [Type]
  }
data Pred =
  IsIn Id [Type]
  deriving (Eq)
data Qual t =
   [Pred]:=>
      t
  deriving (Eq)
data Pat
  = PVar Id
  | PWildcard
  | PAs Id Pat
  | PLit Literal
  | PNpk Id Integer
  | PCon Assumption [Pat]
  | PLazy Pat
data Literal
  = LitInt Integer
  | LitChar Char
  | LitRat Rational
  | LitStr String
data Kind
  = Star
  | Kfun Kind Kind
  deriving (Eq)

class PPrint a where
  pprint :: a -> Doc
  parPprint :: a -> Doc
  parPprint = parens . pprint
  pplist :: [a] -> Doc
  pplist xs = brackets (fsep (punctuate comma (map pprint xs)))

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

class Instantiate t where
  inst :: [Type] -> t -> t

class HasKind t where
  kind :: t -> Kind

class Unify t where
  mgu
    :: Monad m
    => t -> t -> m Subst

class Match t where
  match
    :: Monad m
    => t -> t -> m Subst

instance PPrint Assumption where
  pprint (Assumption i  s) = (text (show i) <+> text ":>:") $$ nest 2 (pprint s)

instance Types Assumption where
  apply s (Assumption i  sc) = Assumption i  (apply s sc)
  tv (Assumption _  sc) = tv sc

instance PPrint Kind where
  pprint = ppkind 0
  parPprint = ppkind 10

instance PPrint a =>
         PPrint [a] where
  pprint = pplist

instance PPrint Char where
  pprint = char
  pplist = text

instance PPrint Integer where
  pprint = integer

instance PPrint Int where
  pprint = int

instance PPrint Float where
  pprint = float

instance PPrint Double where
  pprint = double

instance (PPrint a, PPrint b) =>
         PPrint (a, b) where
  pprint (x, y) = parens (sep [pprint x <> comma, pprint y])

instance (PPrint a, PPrint b, PPrint c) =>
         PPrint (a, b, c) where
  pprint (x, y, z) =
    parens (sep [pprint x <> comma, pprint y <> comma, pprint z])

instance PPrint t =>
         PPrint (Qual t) where
  pprint (ps :=> t) = (pprint ps <+> text ":=>") $$ nest 2 (parPprint t)

instance PPrint Pred where
  pprint (IsIn i [t]) = text "isIn1" <+> text ("c" ++ i) <+> parPprint t
  pprint (IsIn i ts) = text "isIn" <+> text ("c" ++ i) <+> pplist ts

instance Types t =>
         Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `union` tv t

instance Types Pred where
  apply s (IsIn i ts) = IsIn i (apply s ts)
  tv (IsIn _i ts) = tv ts

instance Unify Pred where
  mgu = lift mgu

instance Match Pred where
  match = lift match

instance PPrint Scheme where
  pprint (Forall ks qt) = (text "Forall" <+> pprint ks) $$ nest 2 (parPprint qt)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt

instance Types Type where
  apply s (TVar u) =
    case lookup u s of
      Just t -> t
      Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t = t
  tv (TVar u) = [u]
  tv (TAp l r) = tv l `union` tv r
  tv _ = []

instance Types a =>
         Types [a] where
  apply s = map (apply s)
  tv = nub . concat . map tv

instance Functor TI where
  fmap = liftM

instance Applicative TI where
  (<*>) = Control.Monad.ap
  pure = return

instance Monad TI where
  return x = TI (\s n -> (s, n, x))
  TI f >>= g =
    TI
      (\s n ->
         case f s n of
           (s', m, x) ->
             let TI gx = g x
             in gx s' m)

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n) = ts !! n
  inst _ t = t

instance Instantiate a =>
         Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate t =>
         Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

instance PPrint Type where
  pprint = pptype (0 :: Integer)
  parPprint = pptype (10 :: Integer)

instance PPrint Tyvar where
  pprint (Tyvar v _) = text v

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u) = kind u
  kind (TAp t _) =
    case (kind t) of
      (Kfun _ k) -> k

instance Unify Type where
  mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
  mgu (TVar u) t = varBind u t
  mgu t (TVar u) = varBind u t
  mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
  mgu _ _ = fail "types do not unify"

instance (Unify t, Types t) =>
         Unify [t] where
  mgu (x:xs) (y:ys) = do
    s1 <- mgu x y
    s2 <- mgu (apply s1 xs) (apply s1 ys)
    return (s2 @@ s1)
  mgu [] [] = return nullSubst
  mgu _ _ = fail "lists do not unify"

instance Match Type where
  match (TAp l r) (TAp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  match (TVar u) t
    | kind u == kind t = return (u +-> t)
  match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
  match _ _ = fail "types do not match"

instance Match t =>
         Match [t] where
  match ts ts' = do
    ss <- sequence (zipWith match ts ts')
    foldM merge nullSubst ss

find
  :: Monad m
  => Id -> [Assumption] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((Assumption i'  sc):as) =
  if i == i'
    then return sc
    else find i as

debug
  :: PPrint a
  => String -> a -> b -> b
debug msg val res = trace (msg ++ " = " ++ pretty val ++ "\n") res

enumId :: Int -> Id
enumId n = "v" ++ show n

ppkind :: Int -> Kind -> Doc
ppkind _ Star = text "Star"
ppkind d (Kfun l r) =
  ppParen (d >= 10) (text "Kfun" <+> ppkind 10 l <+> ppkind 0 r)

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _) = do
  v <- newTVar Star
  return ([IsIn "Num" [v]], v)
tiLit (LitStr _) = return ([], tString)
tiLit (LitRat _) = do
  v <- newTVar Star
  return ([IsIn "Fractional" [v]], v)

pretty
  :: PPrint a
  => a -> String
pretty = render . pprint

ppParen :: Bool -> Doc -> Doc
ppParen t x =
  if t
    then parens x
    else x

tiPat :: Pat -> TI ([Pred], [Assumption], Type)
tiPat (PVar i) = do
  v <- newTVar Star
  return ([], [Assumption i (toScheme v)], v)
tiPat PWildcard = do
  v <- newTVar Star
  return ([], [], v)
tiPat (PAs i pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (Assumption i (toScheme t)) : as, t)
tiPat (PLit l) = do
  (ps, t) <- tiLit l
  return (ps, [], t)
tiPat (PNpk i _) = do
  t <- newTVar Star
  return ([IsIn "Integral" [t]], [Assumption i (toScheme t)], t)
tiPat (PCon (Assumption _  sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  return (ps ++ qs, as, t')
tiPat (PLazy pat) = tiPat pat

tiPats :: [Pat] -> TI ([Pred], [Assumption], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  return (ps, as, ts)

predHead :: Pred -> Id
predHead (IsIn i _) = i

lift
  :: Monad m
  => ([Type] -> [Type] -> m a) -> Pred -> Pred -> m a
lift m (IsIn i ts) (IsIn i' ts')
  | i == i' = m ts ts'
  | otherwise = fail "classes differ"

sig :: ClassEnv -> Id -> [Tyvar]
sig ce i =
  case classes ce i of
    Just (vs, _, _) -> vs

super :: ClassEnv -> Id -> [Pred]
super ce i =
  case classes ce i of
    Just (_, is, _) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i =
  case classes ce i of
    Just (_, _, its) -> its

defined :: Maybe a -> Bool
defined (Just _) = True
defined Nothing = False

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c =
  ce
  { classes =
      \j ->
        if i == j
          then Just c
          else classes ce j
  }

initialEnv :: ClassEnv
initialEnv =
  ClassEnv
  {classes = \_ -> fail "class not defined", defaults = [tInteger, tDouble]}

infixr 5 <:>

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do
  ce' <- f ce
  g ce'

addClass :: Id -> [Tyvar] -> [Pred] -> EnvTransformer
addClass i vs ps ce
  | defined (classes ce i) = fail "class already defined"
  | any (not . defined . classes ce . predHead) ps =
    fail "superclass not defined"
  | otherwise = return (modify ce i (vs, ps, []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

atyvar :: Tyvar
atyvar = Tyvar "a" Star

atype :: Type
atype = TVar atyvar

asig :: [Tyvar]
asig = [atyvar]

mtyvar :: Tyvar
mtyvar = Tyvar "m" (Kfun Star Star)

mtype :: Type
mtype = TVar mtyvar

msig :: [Tyvar]
msig = [mtyvar]

addCoreClasses :: EnvTransformer
addCoreClasses =
  addClass "Eq" asig [] <:>
  addClass "Ord" asig [IsIn "Eq" [atype]] <:>
  addClass "Show" asig [] <:>
  addClass "Read" asig [] <:>
  addClass "Bounded" asig [] <:>
  addClass "Enum" asig [] <:>
  addClass "Functor" msig [] <:> addClass "Monad" msig []

addNumClasses :: EnvTransformer
addNumClasses =
  addClass "Num" asig [IsIn "Eq" [atype], IsIn "Show" [atype]] <:>
  addClass "Real" asig [IsIn "Num" [atype], IsIn "Ord" [atype]] <:>
  addClass "Fractional" asig [IsIn "Num" [atype]] <:>
  addClass "Integral" asig [IsIn "Real" [atype], IsIn "Enum" [atype]] <:>
  addClass "RealFrac" asig [IsIn "Real" [atype], IsIn "Fractional" [atype]] <:>
  addClass "Floating" asig [IsIn "Fractional" [atype]] <:>
  addClass "RealFloat" asig [IsIn "RealFrac" [atype], IsIn "Floating" [atype]]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | not (defined (classes ce i)) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = return (modify ce i c)
  where
    its = insts ce i
    qs = [q | (_ :=> q) <- its]
    c = (sig ce i, super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (mgu p q)

exampleInsts :: EnvTransformer
exampleInsts =
  addPreludeClasses <:>
  addInst [] (IsIn "Ord" [tUnit]) <:>
  addInst [] (IsIn "Ord" [tChar]) <:>
  addInst [] (IsIn "Ord" [tInt]) <:>
  addInst
    [IsIn "Ord" [TVar (Tyvar "a" Star)], IsIn "Ord" [TVar (Tyvar "b" Star)]]
    (IsIn "Ord" [pair (TVar (Tyvar "a" Star)) (TVar (Tyvar "b" Star))])

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i ts) = p : concat (map (bySuper ce) supers)
  where
    supers = apply s (super ce i)
    s = zip (sig ce i) ts

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i]
  where
    tryInst (ps :=> h) = do
      u <- match h p
      Just (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p =
  any (p `elem`) (map (bySuper ce) ps) ||
  case byInst ce p of
    Nothing -> False
    Just qs -> all (entail ce ps) qs

simplify :: ([Pred] -> Pred -> Bool) -> [Pred] -> [Pred]
simplify ent = loop []
  where
    loop rs [] = rs
    loop rs (p:ps)
      | ent (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

reduce :: ClassEnv -> [Pred] -> [Pred]
reduce ce = simplify (scEntail ce) . elimTauts ce

elimTauts :: ClassEnv -> [Pred] -> [Pred]
elimTauts ce ps = [p | p <- ps, not (entail ce [] p)]

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- tv qt, v `elem` vs]
    ks = map kind vs'
    s = zip vs' (map TGen [0 ..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

isIn1 :: Id -> Type -> Pred
isIn1 i t = IsIn i [t]

mkInst
  :: Instantiate a
  => [Kind] -> a -> a
mkInst ks = inst ts
  where
    ts = zipWith (\v k -> TVar (Tyvar v k)) vars ks
    vars =
      [[c] | c <- ['a' .. 'z']] ++
      [c : show n | n <- [0 :: Int ..], c <- ['a' .. 'z']]

instances :: [Inst] -> EnvTransformer
instances = foldr1 (<:>) . map (\(ps :=> p) -> addInst ps p)

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge
  :: Monad m
  => Subst -> Subst -> m Subst
merge s1 s2 =
  if agree
    then return (s1 ++ s2)
    else fail "merge fails"
  where
    agree =
      all
        (\v -> apply s1 (TVar v) == apply s2 (TVar v))
        (map fst s1 `intersect` map fst s2)

ap
  :: Foldable t
  => t Expr -> Expr
ap = foldl1 Ap

evar :: Id -> Expr
evar v = (Var v)

elit :: Literal -> Expr
elit l = (Lit l)

econst :: Assumption -> Expr
econst c = (Const c)

elet :: [[(Id, Maybe Scheme, [Alt])]] -> Expr -> Expr
elet e f = foldr Let f (map toBg e)

toBg :: [(Id, Maybe Scheme, [Alt])] -> BindGroup
toBg g =
  ( [(v, t, alts) | (v, Just t, alts) <- g]
  , filter (not . null) [[(v, alts) | (v, Nothing, alts) <- g]])

{-
ecase           = Case
elambda         = Lam
eif             = If
-}
ecase :: Expr -> [(Pat, Expr)] -> Expr
ecase d as =
  elet [[("_case", Nothing, [([p], e) | (p, e) <- as])]] (ap [evar "_case", d])

elambda :: Alt -> Expr
elambda alt = elet [[("_lambda", Nothing, [alt])]] (evar "_lambda")

efail :: Expr
efail = Const (Assumption "FAIL" (Forall [Star] ([] :=> TGen 0)))

esign :: Expr -> Scheme -> Expr
esign e t = elet [[("_val", Just t, [([], e)])]] (evar "_val")

eCompLet :: [[(Id, Maybe Scheme, [Alt])]] -> Expr -> Expr
eCompLet bgs c = elet bgs c

tBool :: Type
tBool = TCon (Tycon "Bool" Star)

tiExpr :: Infer Expr Type
tiExpr _ as (Var i) = do
  sc <- find i as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr _ _ (Const (Assumption _  sc)) = do
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExpr _ _ (Lit l) = do
  (ps, t) <- tiLit l
  return (ps, t)
tiExpr ce as (Ap e f) = do
  (ps, te) <- tiExpr ce as e
  (qs, tf) <- tiExpr ce as f
  t <- newTVar Star
  unify (tf `fn` t) te
  return (ps ++ qs, t)
tiExpr ce as (Let bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, t)
tiExpr ce as (Lam alt) = tiAlt ce as alt
tiExpr ce as (If e e1 e2) = do
  (ps, t) <- tiExpr ce as e
  unify t tBool
  (ps1, t1) <- tiExpr ce as e1
  (ps2, t2) <- tiExpr ce as e2
  unify t1 t2
  return (ps ++ ps1 ++ ps2, t1)
tiExpr ce as (Case e branches) = do
  (ps0, t) <- tiExpr ce as e
  v <- newTVar Star
  let tiBr (pat, f) = do
        (ps, as', t') <- tiPat pat
        unify t t'
        (qs, t'') <- tiExpr ce (as' ++ as) f
        unify v t''
        return (ps ++ qs)
  pss <- mapM tiBr branches
  return (ps0 ++ concat pss, v)

tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExpr ce (as' ++ as) e
  return (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -> [Assumption] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t) (map snd psts)
  return (concat (map fst psts))

split
  :: Monad m
  => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do
  let ps' = reduce ce ps
      (ds, rs) = partition (all (`elem` fs) . tv) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  return (ds, rs \\ rs')

ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]

numClasses :: [Id]
numClasses =
  ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [Id]
stdClasses =
  [ "Eq"
  , "Ord"
  , "Show"
  , "Read"
  , "Bounded"
  , "Enum"
  , "Ix"
  , "Functor"
  , "Monad"
  , "MonadPlus"
  ] ++
  numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  [ t'
  | let is = [i | IsIn i _ <- qs]
        ts = [t | IsIn _ t <- qs]
  , all ([TVar v] ==) ts
  , any (`elem` numClasses) is
  , all (`elem` stdClasses) is
  , t' <- defaults ce
  , all (entail ce []) [IsIn i [t'] | i <- is]
  ]

withDefaults
  :: Monad m
  => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise = return (f vps (map head tss))
  where
    vps = ambiguities ce vs ps
    tss = map (candidates ce) vps

defaultedPreds
  :: Monad m
  => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps _ -> concat (map snd vps))

defaultSubst
  :: Monad m
  => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)

tiExpl :: ClassEnv -> [Assumption] -> Expl -> TI [Pred]
tiExpl ce as (_, sc, alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- getSubst
  let qs' = apply s qs
      t' = apply s t
      fs = tv (apply s as)
      gs = tv t' \\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split ce fs gs ps'
  if sc /= sc'
    then fail "signature too general"
    else if not (null rs)
           then fail "context too weak"
           else return ds

restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where
    simple (_, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assumption]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar Star) bs
  let is = map fst bs
      scs = map toScheme ts
      as' = zipWith Assumption is scs ++ as
      altss = map snd bs
  pss <- sequence (zipWith (tiAlts ce as') altss ts)
  s <- getSubst
  let ps' = apply s (concat pss)
      ts' = apply s ts
      fs = tv (apply s as)
      vss = map tv ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then let gs' = gs \\ tv rs
             scs' = map (quantify gs' . ([] :=>)) ts'
         in return (ds ++ rs, zipWith Assumption is scs')
    else let scs' = map (quantify gs . (rs :=>)) ts'
         in return (ds, zipWith Assumption is scs')

tiBindGroup :: Infer BindGroup [Assumption]
tiBindGroup ce as (es, iss) = do
  let as' = [Assumption v  sc | (v, sc, _alts) <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

tiSeq :: Infer bg [Assumption] -> Infer [bg] [Assumption]
tiSeq _ _ _ [] = return ([], [])
tiSeq ti ce as (bs:bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

newtype TI a =
  TI (Subst -> Int -> (Subst, Int, a))

runTI :: TI a -> a
runTI (TI f) = x
  where
    (_, _, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

trim :: [Tyvar] -> TI ()
trim vs =
  TI
    (\s n ->
       let s' = [(v, t) | (v, t) <- s, v `elem` vs]
           force = length (tv (map snd s'))
       in force `seq` (s', n, ()))

extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s' @@ s, n, ()))

newTVar :: Kind -> TI Type
newTVar k =
  TI
    (\s n ->
       let v = Tyvar (enumId n) k
       in (s, n + 1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return (inst ts qt)

tiProgram :: ClassEnv -> [Assumption] -> Program -> [Assumption]
tiProgram ce as bgs =
  runTI $ do
    (ps, as') <- tiSeq tiBindGroup ce as bgs
    s <- getSubst
    let rs = reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return (apply (s' @@ s) as')

tiBindGroup' :: ClassEnv -> [Assumption] -> BindGroup -> TI ([Pred], [Assumption])
tiBindGroup' ce as bs = do
  (ps, as') <- tiBindGroup ce as bs
  trim (tv (as' ++ as))
  return (ps, as')

tiProgram' :: ClassEnv -> [Assumption] -> Program -> [Assumption]
tiProgram' ce as bgs =
  runTI $ do
    (ps, as') <- tiSeq tiBindGroup' ce as bgs
    s <- getSubst
    let rs = reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return (apply (s' @@ s) as')

pptype
  :: (Num t, Ord t)
  => t -> Type -> Doc
pptype d (TAp (TAp a x) y)
  | a == tArrow =
    ppParen
      (d >= 5)
      (pptype (5 :: Integer) x <+> text "`fn`" <+> pptype (0 :: Integer) y)
pptype d (TAp l r) =
  ppParen
    (d >= 10)
    (text "TAp" <+> pptype (10 :: Integer) l <+> pptype (10 :: Integer) r)
pptype d (TGen n) = ppParen (d >= 10) (text "TGen" <+> int n)
pptype _ t
  | t == tList = text "tList"
  | t == tArrow = text "tArrow"
  | t == tUnit = text "tUnit"
  | t == tTuple2 = text "tTuple2"
  | t == tTuple3 = text "tTuple3"
  | t == tTuple4 = text "tTuple4"
  | t == tTuple5 = text "tTuple5"
  | t == tTuple6 = text "tTuple6"
  | t == tTuple7 = text "tTuple7"
pptype _ (TCon (Tycon i _)) = text ('t' : i)
pptype _ (TVar v) = pprint v

tUnit :: Type
tUnit = TCon (Tycon "()" Star)

tChar :: Type
tChar = TCon (Tycon "Char" Star)

tInt :: Type
tInt = TCon (Tycon "Int" Star)

tInteger :: Type
tInteger = TCon (Tycon "Integer" Star)

tFloat :: Type
tFloat = TCon (Tycon "Float" Star)

tDouble :: Type
tDouble = TCon (Tycon "Double" Star)

tList :: Type
tList = TCon (Tycon "[]" (Kfun Star Star))

tArrow :: Type
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

tTuple2 :: Type
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tTuple3 :: Type
tTuple3 = TCon (Tycon "(,,)" (Kfun Star (Kfun Star (Kfun Star Star))))

tTuple4 :: Type
tTuple4 =
  TCon (Tycon "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))

tTuple5 :: Type
tTuple5 =
  TCon
    (Tycon
       "(,,,,)"
       (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))

tTuple6 :: Type
tTuple6 =
  TCon
    (Tycon
       "(,,,,,)"
       (Kfun
          Star
          (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))

tTuple7 :: Type
tTuple7 =
  TCon
    (Tycon
       "(,,,,,,)"
       (Kfun
          Star
          (Kfun
             Star
             (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))

tString :: Type
tString = list tChar

infixr 4 `fn`

fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list t = TAp tList t

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

varBind
  :: Monad m
  => Tyvar -> Type -> m Subst

varBind u t
  | t == TVar u = return nullSubst
  | u `elem` tv t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return (u +-> t)
