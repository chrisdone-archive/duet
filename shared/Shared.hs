{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Shared application code between commandline and web interface.

module Shared where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Supply
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Duet.Context
import           Duet.Infer
import           Duet.Renamer
import           Duet.Supply
import           Duet.Types

--------------------------------------------------------------------------------
-- Setting the context

-- | Setup the class environment.
setupEnv
  :: (MonadThrow m, MonadSupply Int m)
  => Map Name (Class Type Name ())
  -> [SpecialTypes Name -> m (DataType Type Name)]
  -> m (Builtins Type Name ())
setupEnv env typeMakers = do
  theArrow <- supplyTypeName "(->)"
  theChar <- supplyTypeName "Char"
  theString <- supplyTypeName "String"
  theInteger <- supplyTypeName "Integer"
  theRational <- supplyTypeName "Rational"
  (true, false, boolDataType) <-
    do name <- supplyTypeName "Bool"
       true <- supplyConstructorName "True"
       false <- supplyConstructorName "False"
       pure
         ( true
         , false
         , DataType
             name
             []
             [DataTypeConstructor true [], DataTypeConstructor false []])
  let function =
        (TypeConstructor
           theArrow
           (FunctionKind StarKind (FunctionKind StarKind StarKind)))
  let specialTypes =
        (SpecialTypes
         { specialTypesBool = boolDataType
         , specialTypesChar = TypeConstructor theChar StarKind
         , specialTypesString = TypeConstructor theString StarKind
         , specialTypesFunction = function
         , specialTypesInteger = TypeConstructor theInteger StarKind
         , specialTypesRational = TypeConstructor theRational StarKind
         })
  (numClass, plus, times) <- makeNumClass function
  (negClass, subtract') <- makeNegClass function
  (fracClass, divide) <- makeFracClass function
  (monoidClass) <- makeMonoidClass function
  boolSigs <- dataTypeSignatures specialTypes boolDataType
  typesSigs <-
    fmap
      concat
      (mapM ($ specialTypes) typeMakers >>=
       mapM (dataTypeSignatures specialTypes))
  classSigs <-
    fmap
      concat
      (mapM classSignatures [numClass, negClass, fracClass, monoidClass])
  primopSigs <- makePrimOps specialTypes
  let signatures = boolSigs <> classSigs <> primopSigs <> typesSigs
      specialSigs =
        SpecialSigs
        { specialSigsTrue = true
        , specialSigsFalse = false
        , specialSigsPlus = plus
        , specialSigsSubtract = subtract'
        , specialSigsTimes = times
        , specialSigsDivide = divide
        }
      specials = Specials specialSigs specialTypes
  stringMonoid <-
    makeInst
      specials
      (IsIn
         (className monoidClass)
         [ConstructorType (specialTypesString specialTypes)])
      [ ( "append"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopStringAppend)))
      , ( "empty"
        , Alternative
            ()
            []
            (LiteralExpression () (StringLiteral "")))
      ]
  numInt <-
    makeInst
      specials
      (IsIn
         (className numClass)
         [ConstructorType (specialTypesInteger specialTypes)])
      [ ( "times"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopIntegerTimes)))
      , ( "plus"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopIntegerPlus)))
      ]
  negInt <-
    makeInst
      specials
      (IsIn
         (className negClass)
         [ConstructorType (specialTypesInteger specialTypes)])
      [ ( "subtract"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopIntegerSubtract)))
      ]
  numRational <-
    makeInst
      specials
      (IsIn
         (className numClass)
         [ConstructorType (specialTypesRational specialTypes)])
      [ ( "times"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopRationalTimes)))
      , ( "plus"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopRationalPlus)))
      ]
  negRational <-
    makeInst
      specials
      (IsIn
         (className negClass)
         [ConstructorType (specialTypesRational specialTypes)])
      [ ( "subtract"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopRationalSubtract)))
      ]
  fracRational <-
    makeInst
      specials
      (IsIn
         (className fracClass)
         [ConstructorType (specialTypesRational specialTypes)])
      [ ( "divide"
        , Alternative
            ()
            []
            (VariableExpression
               ()
               (PrimopName PrimopRationalDivide)))
      ]
  env' <-
    let update =
          addClass numClass >=>
          addClass negClass >=>
          addClass fracClass >=>
          addClass monoidClass >=>
          addInstance numInt >=>
          addInstance negInt >=>
          addInstance stringMonoid >=>
          addInstance fracRational >=>
          addInstance negRational >=> addInstance numRational
    in update env
  pure
    Builtins
    { builtinsSpecialSigs = specialSigs
    , builtinsSpecialTypes = specialTypes
    , builtinsSignatures = signatures
    , builtinsTypeClasses = env'
    }

--------------------------------------------------------------------------------
-- Builtin classes and primops

makePrimOps
  :: (MonadSupply Int m)
  => SpecialTypes Name -> m [TypeSignature Type Name Name]
makePrimOps SpecialTypes {..} = do
  let sigs =
        map
          ((\case
              PrimopIntegerPlus ->
                TypeSignature
                  (PrimopName PrimopIntegerPlus)
                  (toScheme (integer --> integer --> integer))
              PrimopIntegerSubtract ->
                TypeSignature
                  (PrimopName PrimopIntegerSubtract)
                  (toScheme (integer --> integer --> integer))
              PrimopIntegerTimes ->
                TypeSignature
                  (PrimopName PrimopIntegerTimes)
                  (toScheme (integer --> integer --> integer))
              PrimopRationalDivide ->
                TypeSignature
                  (PrimopName PrimopRationalDivide)
                  (toScheme (rational --> rational --> rational))
              PrimopRationalPlus ->
                TypeSignature
                  (PrimopName PrimopRationalPlus)
                  (toScheme (rational --> rational --> rational))
              PrimopRationalSubtract ->
                TypeSignature
                  (PrimopName PrimopRationalSubtract)
                  (toScheme (rational --> rational --> rational))
              PrimopRationalTimes ->
                TypeSignature
                  (PrimopName PrimopRationalTimes)
                  (toScheme (rational --> rational --> rational))
              PrimopStringAppend ->
                TypeSignature
                  (PrimopName PrimopStringAppend)
                  (toScheme (string --> string --> string))))
          [minBound .. maxBound]
  pure sigs
  where
    integer = ConstructorType specialTypesInteger
    rational = ConstructorType specialTypesRational
    string = ConstructorType specialTypesString
    infixr 1 -->
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b =
      ApplicationType
        (ApplicationType (ConstructorType specialTypesFunction) a)
        b

makeNumClass :: MonadSupply Int m => TypeConstructor Name -> m (Class Type Name l, Name, Name)
makeNumClass function = do
  a <- fmap (\n -> TypeVariable n StarKind) (supplyTypeName "a")
  let a' = VariableType a
  plus <- supplyMethodName "plus"
  times <- supplyMethodName "times"
  cls <-
    makeClass
      "Num"
      [a]
      [ (plus, Forall [a] (Qualified [] (a' --> a' --> a')))
      , (times, Forall [a] (Qualified [] (a' --> a' --> a')))
      ]
  pure (cls, plus, times)
  where
    infixr 1 -->
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b = ApplicationType (ApplicationType (ConstructorType function) a) b

makeNegClass :: MonadSupply Int m => TypeConstructor Name -> m (Class Type Name l, Name)
makeNegClass function = do
  a <- fmap (\n -> TypeVariable n StarKind) (supplyTypeName "a")
  let a' = VariableType a
  negate' <- supplyMethodName "negate"
  subtract' <- supplyMethodName "subtract"
  abs' <- supplyMethodName "abs"
  cls <-
    makeClass
      "Neg"
      [a]
      [ (negate', Forall [a] (Qualified [] (a' --> a' --> a')))
      , (subtract', Forall [a] (Qualified [] (a' --> a' --> a')))
      , (abs', Forall [a] (Qualified [] (a' --> a')))
      ]
  pure (cls, subtract')
  where
    infixr 1 -->
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b = ApplicationType (ApplicationType (ConstructorType function) a) b

makeFracClass :: MonadSupply Int m => TypeConstructor Name -> m (Class Type Name l, Name)
makeFracClass function = do
  a <- fmap (\n -> TypeVariable n StarKind) (supplyTypeName "a")
  let a' = VariableType a
  divide <- supplyMethodName "divide"
  recip' <- supplyMethodName "recip"
  cls <-
    makeClass
      "Fractional"
      [a]
      [ (divide, Forall [a] (Qualified [] (a' --> a' --> a')))
      , (recip', Forall [a] (Qualified [] (a' --> a')))
      ]
  pure (cls, divide)
  where
    infixr 1 -->
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b = ApplicationType (ApplicationType (ConstructorType function) a) b

makeMonoidClass :: MonadSupply Int m => TypeConstructor Name -> m (Class Type Name l)
makeMonoidClass function = do
  a <- fmap (\n -> TypeVariable n StarKind) (supplyTypeName "a")
  let a' = VariableType a
  append <- supplyMethodName "append"
  empty <- supplyMethodName "empty"
  cls <-
    makeClass
      "Monoid"
      [a]
      [ (append, Forall [a] (Qualified [] (a' --> a' --> a')))
      , (empty, Forall [a] (Qualified [] (a')))
      ]
  pure cls
  where
    infixr 1 -->
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b = ApplicationType (ApplicationType (ConstructorType function) a) b
