-- |

module Classes where

import Control.Monad hiding (ap)
import THIH

exampleInsts :: (ClassEnvironment -> Maybe ClassEnvironment)
exampleInsts =
  addPreludeClasses >=>
  addInst [] (IsIn "Ord" [tUnit]) >=>
  addInst [] (IsIn "Ord" [tChar]) >=>
  addInst [] (IsIn "Ord" [tInt]) >=>
  addInst
    [IsIn "Ord" [VariableType (TypeVariable "a" StarKind)], IsIn "Ord" [VariableType (TypeVariable "b" StarKind)]]
    (IsIn "Ord" [pair (VariableType (TypeVariable "a" StarKind)) (VariableType (TypeVariable "b" StarKind))])

addPreludeClasses :: (ClassEnvironment -> Maybe ClassEnvironment)
addPreludeClasses = addCoreClasses >=> addNumClasses

addCoreClasses :: (ClassEnvironment -> Maybe ClassEnvironment)
addCoreClasses =
  addClass "Eq" asig [] >=>
  addClass "Ord" asig [IsIn "Eq" [atype]] >=>
  addClass "Show" asig [] >=>
  addClass "Read" asig [] >=>
  addClass "Bounded" asig [] >=>
  addClass "Enum" asig [] >=>
  addClass "Functor" msig [] >=> addClass "Monad" msig []

addNumClasses :: (ClassEnvironment -> Maybe ClassEnvironment)
addNumClasses =
  addClass "Num" asig [IsIn "Eq" [atype], IsIn "Show" [atype]] >=>
  addClass "Real" asig [IsIn "Num" [atype], IsIn "Ord" [atype]] >=>
  addClass "Fractional" asig [IsIn "Num" [atype]] >=>
  addClass "Integral" asig [IsIn "Real" [atype], IsIn "Enum" [atype]] >=>
  addClass "RealFrac" asig [IsIn "Real" [atype], IsIn "Fractional" [atype]] >=>
  addClass "Floating" asig [IsIn "Fractional" [atype]] >=>
  addClass "RealFloat" asig [IsIn "RealFrac" [atype], IsIn "Floating" [atype]]

atyvar :: TypeVariable
atyvar = TypeVariable "a" StarKind

atype :: Type
atype = VariableType atyvar

asig :: [TypeVariable]
asig = [atyvar]

mtyvar :: TypeVariable
mtyvar = TypeVariable "m" (FunctionKind StarKind StarKind)

mtype :: Type
mtype = VariableType mtyvar

msig :: [TypeVariable]
msig = [mtyvar]

isIn1 :: Identifier -> Type -> Predicate
isIn1 i t = IsIn i [t]

mkInst
  :: Instantiate a
  => [Kind] -> a -> a
mkInst ks = instantiate ts
  where
    ts = zipWith (\v k -> VariableType (TypeVariable v k)) vars ks
    vars =
      map Identifier ([[c] | c <- ['a' .. 'z']] ++
                      [c : show n | n <- [0 :: Int ..], c <- ['a' .. 'z']])

ap
  :: Foldable t
  => t Expression -> Expression
ap = foldl1 ApplicationExpression

evar :: Identifier -> Expression
evar v = (VariableExpression v)

elit :: Literal -> Expression
elit l = (LiteralExpression l)

econst :: Assumption -> Expression
econst c = (ConstantExpression c)

elet :: [[(Identifier, Maybe Scheme, [Alternative])]] -> Expression -> Expression
elet e f = foldr LetExpression f (map toBg e)

ecase :: Expression -> [(Pattern, Expression)] -> Expression
ecase d as =
  elet [[("_case", Nothing, [Alternative [p] e | (p, e) <- as])]] (ap [evar "_case", d])

elambda :: Alternative -> Expression
elambda alt = elet [[("_lambda", Nothing, [alt])]] (evar "_lambda")

efail :: Expression
efail = ConstantExpression (Assumption "FAIL" (Forall [StarKind] (Qualified [] (GenericType 0))))

esign :: Expression -> Scheme -> Expression
esign e t = elet [[("_val", Just t, [(Alternative [] e)])]] (evar "_val")

eCompLet :: [[(Identifier, Maybe Scheme, [Alternative])]] -> Expression -> Expression
eCompLet bgs c = elet bgs c

toBg :: [(Identifier, Maybe Scheme, [Alternative])] -> BindGroup
toBg g =
  BindGroup
  { bindGroupExplicitlyTypedBindings =
      [ExplicitlyTypedBinding v t alts | (v, Just t, alts) <- g]
  , bindGroupImplicitlyTypedBindings =
      filter
        (not . null)
        [[ImplicitlyTypedBinding v alts | (v, Nothing, alts) <- g]]
  }

tUnit :: Type
tUnit = ConstructorType (TypeConstructor "()" StarKind)



tInt :: Type
tInt = ConstructorType (TypeConstructor "Int" StarKind)

tInteger :: Type
tInteger = ConstructorType (TypeConstructor "Integer" StarKind)

tFloat :: Type
tFloat = ConstructorType (TypeConstructor "Float" StarKind)

tDouble :: Type
tDouble = ConstructorType (TypeConstructor "Double" StarKind)

tArrow :: Type
tArrow = ConstructorType (TypeConstructor "(->)" (FunctionKind StarKind (FunctionKind StarKind StarKind)))

tTuple2 :: Type
tTuple2 = ConstructorType (TypeConstructor "(,)" (FunctionKind StarKind (FunctionKind StarKind StarKind)))

tTuple3 :: Type
tTuple3 = ConstructorType (TypeConstructor "(,,)" (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind StarKind))))

tTuple4 :: Type
tTuple4 =
  ConstructorType (TypeConstructor "(,,,)" (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind StarKind)))))

tTuple5 :: Type
tTuple5 =
  ConstructorType
    (TypeConstructor
       "(,,,,)"
       (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind StarKind))))))

tTuple6 :: Type
tTuple6 =
  ConstructorType
    (TypeConstructor
       "(,,,,,)"
       (FunctionKind
          StarKind
          (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind StarKind)))))))

tTuple7 :: Type
tTuple7 =
  ConstructorType
    (TypeConstructor
       "(,,,,,,)"
       (FunctionKind
          StarKind
          (FunctionKind
             StarKind
             (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind (FunctionKind StarKind StarKind))))))))



infixr 4 `fn`

fn :: Type -> Type -> Type
a `fn` b = ApplicationType (ApplicationType tArrow a) b



pair :: Type -> Type -> Type
pair a b = ApplicationType (ApplicationType tTuple2 a) b
