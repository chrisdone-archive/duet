{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Typeable
import           Duet.Context
import           Duet.Errors
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Resolver
import           Duet.Stepper
import           Duet.Supply
import           Duet.Types
import           Reflex.Dom

--------------------------------------------------------------------------------
-- Constants

defaultInput = "main = 1 * 2"

maxSteps = 100

inputName = "<interactive>"

mainFunc = "main"

--------------------------------------------------------------------------------
-- Main entry point

main =
  mainWidget
    (do makeHeader
        result <-
          container
            (row
               (do input <- col 6 makeSourceInput
                   result <- mapDyn compileAndRun input
                   col 6 (makeStepsBox result)
                   pure result))
        makeErrorsBox result)

makeHeader =
  container
    (row
       (col
          12
          (do el "h1" (text "Duet (delta)")
              el
                "p"
                (text
                   "Duet is a dialect of Haskell. This is a demonstration page with an in-browser type-checker and interpreter."))))

makeSourceInput = do
  input <-
    do el "h2" (text "Input program")
       el
         "p"
         (textArea
            def
            { _textAreaConfig_initialValue = defaultInput
            , _textAreaConfig_attributes =
                constDyn
                  (M.fromList
                     [ ("class", "form-control")
                     , ("rows", "15")
                     , ("style", "font-family: monospace")
                     ])
            })
  debouncedInputEv <- debounce 0.5 (updated (_textArea_value input))
  foldDyn const defaultInput debouncedInputEv

makeStepsBox result = do
  stepsText <-
    foldDyn
      (\result last -> either (const last) (printSteps . Right) result)
      initialValue
      (updated result)
  el "h2" (text "Steps")
  attributes <-
    mapDyn
      (either
         (const
            (M.fromList
               (defaultAttributes ++
                [("style", "font-family: monospace; color:#aaa")])))
         (const
            (M.fromList
               (defaultAttributes ++ [("style", "font-family: monospace")]))))
      result
  el
    "p"
    (textArea
       (def :: TextAreaConfig Spider)
       { _textAreaConfig_initialValue = initialValue
       , _textAreaConfig_attributes = attributes
       , _textAreaConfig_setValue = updated stepsText
       })
  where
    initialValue = printSteps (compileAndRun defaultInput)
    defaultAttributes = [("class", "form-control"), ("rows", "15")]

makeErrorsBox result = do
  errorAttrs <-
    mapDyn
      (M.fromList .
       either
         (const [("class", "container")])
         (const [("style", "display: none")]))
      result
  errorMessage <- mapDyn (either displayException (const "")) result
  elDynAttr
    "div"
    errorAttrs
    (row
       (col
          12
          (elClass
             "div"
             "alert alert-danger"
             (elAttr
                "p"
                (M.fromList [("style", "white-space: pre")])
                (dynText errorMessage)))))

--------------------------------------------------------------------------------
-- Shared functions

compileAndRun text =
  evalSupplyT
    (do (binds, context) <- createContext inputName (T.pack text)
        execWriterT (runStepper maxSteps context binds mainFunc))
    [1 ..] :: Either SomeException [String]

printSteps = either (const "") (unlines . reverse)

--------------------------------------------------------------------------------
-- Bootstrap short-hands

container = elClass "div"  "container"
row = elClass "div"  "row"
col n = elClass "div" ("col-md-" ++ show (n :: Int))

--------------------------------------------------------------------------------
-- Context setup

data ContextException = ContextException (SpecialTypes Name) SomeException
  deriving (Show, Typeable)

instance Exception ContextException where
  displayException (ContextException specialTypes (SomeException se)) =
    maybe
      (maybe
         (maybe
            (maybe
               (maybe
                  (displayException se)
                  (displayRenamerException specialTypes)
                  (cast se))
               (displayInferException specialTypes)
               (cast se))
            (displayStepperException specialTypes)
            (cast se))
         (displayResolveException specialTypes)
         (cast se))
      displayParseException
      (cast se)

-- | Create a context of all renamed, checked and resolved code.
createContext
  :: (MonadSupply Int m, MonadThrow m, MonadCatch m)
  => String
  -> Text
  -> m ([BindGroup Type Name (TypeSignature Type Name Location)], Context Type Name Location)
createContext file text = do
  do builtins <- setupEnv mempty
     let specials = builtinsSpecials builtins
     catch
       (do decls <- parseText file text
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
             resolveTypeClasses
               typeCheckedClasses
               (builtinsSpecialTypes builtins)
           resolvedBindGroups <-
             mapM
               (resolveBindGroup
                  resolvedTypeClasses
                  (builtinsSpecialTypes builtins))
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
           pure (resolvedBindGroups, context))
       (throwM . ContextException (builtinsSpecialTypes builtins))

--------------------------------------------------------------------------------
-- Stepper

-- | Run the substitution model on the code.
runStepper
  :: (MonadWriter [String] m, MonadSupply Int m, MonadThrow m)
  => Int
  -> Context Type Name Location
  -> [BindGroup Type Name (TypeSignature Type Name Location)]
  -> String
  -> m ()
runStepper maxSteps context bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  fix
    (\loopy count lastString e -> do
       e' <- expandSeq1 context bindGroups' e
       let string = printExpression (defaultPrint) e
       when
         (string /= lastString && (True || cleanExpression e))
         (tell [string])
       if (fmap (const ()) e' /= fmap (const ()) e) && count < maxSteps
         then do
           renameExpression
             (contextSpecials context)
             (contextScope context)
             (contextDataTypes context)
             e' >>=
             loopy (count + 1) string
         else when (count >= maxSteps)
                   (tell ["<max steps exceeded: " ++ show maxSteps ++ ">"]))
    1
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
