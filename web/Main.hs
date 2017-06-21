{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Arrow
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
import           Data.Tuple
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

maxSteps = 200

inputName = "<interactive>"

mainFunc = "main"

exampleInputs =
  [("Arithmetic",arithmeticSource)
  ,("Factorial",facSource)
  ,("Lists",listsSource)
  ,("Folds", foldsSource)
  ,("Currying", curryinglistsSource)
  ,("Monad", monadSource)
  ,("Read/Show",readshowSource)
  ,("Lists factorial",listsFactorialSource)]

--------------------------------------------------------------------------------
-- Main entry point

main =
  mainWidget
    (do makeHeader
        container
          (row
             (do (currentSource, result) <-
                   col
                     6
                     (do el "h2" (text "Input program")
                         currentSource <- examples
                         input <- makeSourceInput currentSource
                         result <- mapDyn compileAndRun input
                         makeErrorsBox result
                         pure (currentSource, result))
                 col 6 (do el "h2" (text "Steps")
                           currentMode <- stepmodes
                           makeStepsBox currentMode currentSource result)
                 pure ())))

stepmodes =
  el
    "p"
    (do dropper <-
          dropdown
            True
            (constDyn (M.fromList [(True, "Complete output"),(False, "Concise output")]))
            (def :: DropdownConfig Spider Bool)
        pure (_dropdown_value dropper))

examples = do
  el
    "p"
    (do dropper <-
          dropdown
            (fromMaybe "" (listToMaybe (fmap snd exampleInputs)))
            (constDyn (M.fromList (map swap exampleInputs)))
            (def :: DropdownConfig Spider String)
        pure (_dropdown_value dropper))

makeHeader =
  container
    (row
       (col
          12
          (do el
                "h1"
                (elAttr
                   "img"
                   (M.fromList [("src", "duet.png"), ("style", "height: 3em; margin-bottom: 0.5em")])
                   (return ()))
              el
                "p"
                (text
                   "Duet is an educational dialect of Haskell aimed at interactivity. This is a demonstration page of the work-in-progress implementation, compiled to JavaScript, consisting of a type-checker and interpreter."))))

makeSourceInput currentSource = do
  defInput <- sample (current currentSource)
  input <-
    el
      "p"
      (textArea
         (def :: TextAreaConfig Spider)
         { _textAreaConfig_initialValue = defInput
         , _textAreaConfig_setValue = updated currentSource
         , _textAreaConfig_attributes =
             constDyn
               (M.fromList
                  [ ("class", "form-control")
                  , ("rows", "15")
                  , ("style", "font-family: monospace")
                  ])
         })
  debouncedInputEv <- debounce 0.5 (updated (_textArea_value input))
  foldDyn const defInput debouncedInputEv

makeStepsBox currentMode currentSource result = do
  initialMode <- sample (current currentMode)
  initialValue <- fmap (initialValue initialMode) (sample (current currentSource))
  modesAndResults <- combineDyn (,) currentMode result
  stepsText <-
    foldDyn
      (\(mode, result) last ->
         either (const last) (printSteps mode . Right) result)
      initialValue
      (updated modesAndResults)
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
    initialValue complete = printSteps complete . compileAndRun
    defaultAttributes =
      [("readonly", "readonly"), ("class", "form-control"), ("rows", "15")]

makeErrorsBox result = do
  errorAttrs <-
    mapDyn
      (M.fromList .
       either
         (const [("class", "alert alert-danger")])
         (const [("style", "display: none")]))
      result
  errorMessage <- mapDyn (either displayException (const "")) result
  elDynAttr
    "div"
    errorAttrs
    (elAttr
       "p"
       (M.fromList [("style", "white-space: pre-wrap;")])
       (dynText errorMessage))

--------------------------------------------------------------------------------
-- Shared functions

compileAndRun text =
  evalSupplyT
    (do (binds, context) <- createContext inputName (T.pack text)
        execWriterT (runStepper maxSteps context binds mainFunc))
    [1 ..] :: Either SomeException [Expression Type Name ()]

printSteps complete =
  either
    (const "")
    (unlines . map (printExpression defaultPrint) . filter mode . reverse)
  where
    mode =
      if complete
        then const True
        else cleanExpression

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
  :: (MonadWriter [Expression Type Name ()] m, MonadSupply Int m, MonadThrow m)
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
         (string /= lastString)
         (tell [fmap (const ()) e])
       if (fmap (const ()) e' /= fmap (const ()) e) && count < maxSteps
         then do
           renameExpression
             (contextSpecials context)
             (contextScope context)
             (contextDataTypes context)
             e' >>=
             loopy (count + 1) string
         else pure ())
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

--------------------------------------------------------------------------------
-- Example sources

listsSource =
  "data List a = Nil | Cons a (List a)\n\
   \data Tuple a b = Tuple a b\n\
   \id = \\x -> x\n\
   \not = \\p -> if p then False else True\n\
   \foldr = \\cons nil l ->\n\
   \  case l of\n\
   \    Nil -> nil\n\
   \    Cons x xs -> cons x (foldr cons nil xs)\n\
   \map = \\f xs ->\n\
   \  case xs of\n\
   \    Nil -> Nil\n\
   \    Cons x xs -> Cons (f x) (map f xs)\n\
   \zip = \\xs ys ->\n\
   \  case Tuple xs ys of\n\
   \    Tuple Nil _ -> Nil\n\
   \    Tuple _ Nil -> Nil\n\
   \    Tuple (Cons x xs1) (Cons y ys1) ->\n\
   \      Cons (Tuple x y) (zip xs1 ys1)\n\
   \list = (Cons True (Cons False Nil))\n\
   \main = zip list list"

monadSource =
 "class Monad (m :: Type -> Type) where\n\
  \  bind :: m a -> (a -> m b) -> m b\n\
  \class Applicative (f :: Type -> Type) where\n\
  \  pure :: a -> f a\n\
  \class Functor (f :: Type -> Type) where\n\
  \  map :: (a -> b) -> f a -> f b\n\
  \data Maybe a = Nothing | Just a\n\
  \instance Functor Maybe where\n\
  \  map =\n\
  \    \\f m ->\n\
  \      case m of\n\
  \        Nothing -> Nothing\n\
  \        Just a -> Just (f a)\n\
  \instance Monad Maybe where\n\
  \  bind =\n\
  \    \\m f ->\n\
  \      case m of\n\
  \        Nothing -> Nothing\n\
  \        Just v -> f v\n\
  \instance Applicative Maybe where\n\
  \  pure = \\v -> Just v\n\n\
 \main = bind (pure 1) (\\i -> Just (i * 2))"

foldsSource =
  "data List a = Nil | Cons a (List a)\n\
   \foldr = \\f z l ->\n\
   \  case l of\n\
   \    Nil -> z\n\
   \    Cons x xs -> f x (foldr f z xs)\n\
   \foldl = \\f z l ->\n\
   \  case l of\n\
   \    Nil -> z\n\
   \    Cons x xs -> foldl f (f z x) xs\n\
   \list = (Cons True (Cons False Nil))\n\
   \main = foldr _f _nil list"

facSource = "go = \\n res ->\n\
             \  case n of\n\
             \    0 -> res\n\
             \    n -> go (n - 1) (res * n)\n\
             \\n\
             \fac = \\n -> go n 1\n\
             \\n\
             \factorial = \\n ->\n\
             \  case n of\n\
             \    0 -> 1\n\
             \    n -> n * factorial (n - 1)\n\
             \\n\
             \main = fac 5"

readshowSource = "class Reader a where\n\
                  \  reader :: List Ch -> a\n\
                  \class Shower a where\n\
                  \  shower :: a -> List Ch\n\
                  \instance Shower Nat where\n\
                  \  shower = \\n ->\n\
                  \    case n of\n\
                  \      Zero -> Cons Z Nil\n\
                  \      Succ n -> Cons S (shower n)\n\
                  \data Nat = Succ Nat | Zero\n\
                  \instance Reader Nat where\n\
                  \  reader = \\cs ->\n\
                  \    case cs of\n\
                  \      Cons Z Nil -> Zero\n\
                  \      Cons S xs  -> Succ (reader xs)\n\
                  \      _ -> Zero\n\
                  \data List a = Nil | Cons a (List a)\n\
                  \data Ch = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z\n\
                  \class Equal a where\n\
                  \  equal :: a -> a -> Bool\n\
                  \instance Equal Nat where\n\
                  \  equal =\n\
                  \    \\a b ->\n\
                  \      case a of\n\
                  \        Zero ->\n\
                  \          case b of\n\
                  \            Zero -> True\n\
                  \            _ -> False\n\
                  \        Succ n ->\n\
                  \          case b of\n\
                  \            Succ m -> equal n m\n\
                  \            _ -> False\n\
                  \        _ -> False\n\
                  \not = \\b -> case b of\n\
                  \              True -> False\n\
                  \              False -> True\n\
                  \\n\
                  \notEqual :: Equal a => a -> a -> Bool\n\
                  \notEqual = \\x y -> not (equal x y)\n\
                  \\n\
                  \main = equal (reader (shower (Succ Zero))) (Succ Zero)\n\
                  \"

arithmeticSource = "main = 2 * (10 - (5 + -3))"

curryinglistsSource = "data List a = Nil | Cons a (List a)\n\
                       \map = \\f xs ->\n\
                       \  case xs of\n\
                       \    Nil -> Nil\n\
                       \    Cons x xs -> Cons (f x) (map f xs)\n\
                       \multiply = \\x y -> x * y\n\
                       \doubleAll = map (multiply 2)\n\
                       \main = doubleAll (Cons 1 (Cons 2 Nil))"

listsFactorialSource = "data List a = Nil | Cons a (List a)\n\
                        \id = \\x -> x\n\
                        \foldr = \\cons nil l ->\n\
                        \  case l of\n\
                        \    Nil -> nil\n\
                        \    Cons x xs -> cons x (foldr cons nil xs)\n\
                        \enumFromTo = \\from to ->\n\
                        \  case to of\n\
                        \   0 -> Nil\n\
                        \   _ -> Cons from (enumFromTo (from + 1) (to - 1))\n\
                        \fac = \\n -> foldr (\\x g n -> g (x * n)) id (enumFromTo 1 n) 1\n\
                        \main = fac 3"
