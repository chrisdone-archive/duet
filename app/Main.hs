{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Simple compiler and stepper.

module Main where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Duet.Context
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Resolver
import           Duet.Stepper
import           Duet.Supply
import           Duet.Types
import           System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:is) -> do
      text <- T.readFile file
      evalSupplyT
        (do (binds, context) <- createContext "<interactive>" text
            maybe (return ()) (runStepper context binds) (listToMaybe is))
        [1 ..]
    _ -> error "usage: duet <file>"

--------------------------------------------------------------------------------
-- Context setup

-- | Create a context of all renamed, checked and resolved code.
createContext
  :: (MonadSupply Int m, MonadThrow m)
  => String
  -> Text
  -> m ([BindGroup Type Name (TypeSignature Type Name Location)], Context Type Name Location)
createContext file text = do
  do decls <- parseText file text
     builtins <- setupEnv mempty
     let specials = builtinsSpecials builtins
     -- Renaming
     (typeClasses, signatures, renamedBindings, scope, dataTypes) <-
       renameEverything decls specials builtins
     -- Type class definition
     addedTypeClasses <- addClasses builtins typeClasses
     -- Type checking
     (bindGroups, typeCheckedClasses) <-
       typeCheckModule
         addedTypeClasses
         signatures
         (builtinsSpecialTypes builtins)
         renamedBindings
     -- Type class resolution
     resolvedTypeClasses <-
       resolveTypeClasses typeCheckedClasses (builtinsSpecialTypes builtins)
     resolvedBindGroups <-
       mapM
         (resolveBindGroup resolvedTypeClasses (builtinsSpecialTypes builtins))
         bindGroups
     -- Create a context of everything
     let context =
           Context
           { contextSpecialSigs = builtinsSpecialSigs builtins
           , contextSpecialTypes = builtinsSpecialTypes builtins
           , contextSignatures = signatures
           , contextScope = scope
           , contextTypeClasses = resolvedTypeClasses
           , contextDataTypes = dataTypes
           }
     pure (resolvedBindGroups, context)

--------------------------------------------------------------------------------
-- Stepper

-- | Run the substitution model on the code.
runStepper
  :: (MonadIO m, MonadSupply Int m, MonadThrow m)
  => Context Type Name Location
  -> [BindGroup Type Name (TypeSignature Type Name Location)]
  -> String
  -> m ()
runStepper context bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  fix
    (\loopy lastString e -> do
       e' <- expandSeq1 context bindGroups' e
       let string = printExpression (defaultPrint) e
       when
         (string /= lastString && (True || cleanExpression e))
         (liftIO (putStrLn string))
       if fmap (const ()) e' /= fmap (const ()) e
         then do
           renameExpression
             (contextSpecials context)
             (contextScope context)
             (contextDataTypes context)
             e' >>=
             loopy string
         else pure ())
    ""
    e0

-- | Filter out expressions with intermediate case, if and immediately-applied lambdas.
cleanExpression :: Expression Type i l -> Bool
cleanExpression =
  \case
    CaseExpression {} -> False
    IfExpression {} -> False
    e0
      | (LambdaExpression {}, args) <- fargs e0 -> null args
    ApplicationExpression _ f x -> cleanExpression f && cleanExpression x
    _ -> True

--------------------------------------------------------------------------------
-- Setting the context

-- | Setup the class environment.
setupEnv
  :: (MonadThrow m, MonadSupply Int m)
  => Map Name (Class Type Name Location)
  -> m (Builtins Type Name Location)
setupEnv env = do
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
  boolSigs <- dataTypeSignatures specialTypes boolDataType
  classSigs <-
    fmap concat (mapM classSignatures [numClass, negClass, fracClass])
  primopSigs <- makePrimOps specialTypes
  let signatures = boolSigs <> classSigs <> primopSigs
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
  numInt <-
    makeInst
      specials
      (IsIn
         (className numClass)
         [ConstructorType (specialTypesInteger specialTypes)])
      [ ( "times"
        , Alternative
            (Location 0 0 0 0)
            []
            (VariableExpression
               (Location 0 0 0 0)
               (PrimopName PrimopIntegerTimes)))
      , ( "plus"
        , Alternative
            (Location 0 0 0 0)
            []
            (VariableExpression
               (Location 0 0 0 0)
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
            (Location 0 0 0 0)
            []
            (VariableExpression
               (Location 0 0 0 0)
               (PrimopName PrimopIntegerSubtract)))
      ]
  env' <-
    let update =
          addClass numClass >=>
          addClass negClass >=>
          addClass fracClass >=> addInstance numInt >=> addInstance negInt
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
  :: (MonadSupply Int m, MonadThrow m)
  => SpecialTypes Name -> m [TypeSignature Type Name Name]
makePrimOps SpecialTypes {..} = do
  let sigs =
        [ TypeSignature
            (PrimopName PrimopIntegerSubtract)
            (toScheme (integer --> integer --> integer))
        , TypeSignature
            (PrimopName PrimopIntegerTimes)
            (toScheme (integer --> integer --> integer))
        , TypeSignature
            (PrimopName PrimopIntegerPlus)
            (toScheme (integer --> integer --> integer))
        ]
  pure sigs
  where
    integer = ConstructorType specialTypesInteger
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
