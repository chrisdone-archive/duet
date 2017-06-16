{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Resolver
import           Duet.Stepper
import           Duet.Supply
import           Duet.Tokenizer
import           Duet.Types
import           System.Environment
import           System.Exit
import           Text.EditDistance

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:is) -> do
      text <- T.readFile file
      compileStepText "<interactive>" (listToMaybe is) text
    _ -> error "usage: duet <file>"

compileStepText :: String -> Maybe String -> Text -> IO ()
compileStepText file i text =
  case parseText file text of
    Left e -> error (show e)
    Right decls -> do
      putStrLn "-- Parsed code:"
      mapM_
        (\case
           BindGroupDecl (BindGroup _ is) ->
             mapM_
               (mapM_ (putStrLn . printImplicitlyTypedBinding (defaultPrint)))
               is
           _ -> return ())
        decls
      ((specialSigs, specialTypes, bindGroups, signatures, subs, typeClassEnv, types), supplies) <-
        runTypeChecker decls
      let specials = Specials specialSigs specialTypes
      putStrLn "-- Type-checked bindings:"
      mapM_
        (\(BindGroup es is) -> do
           mapM_
             (putStrLn . printExplicitlyTypedBinding defaultPrint specialTypes)
             es
           mapM_
             (mapM_ (putStrLn . printImplicitlyTypedBinding (defaultPrint)))
             is)
        bindGroups
      putStrLn "-- With type-annotations:"
      mapM_
        (\(BindGroup es is) -> do
           (mapM_
              (putStrLn .
               printExplicitlyTypedBinding
                 defaultPrint
                 {printTypes = \x -> Just (specialTypes, fmap (const ()) x)}
                 specialTypes)
              es)
           mapM_
             (mapM_
                (putStrLn .
                 printImplicitlyTypedBinding
                   defaultPrint
                   {printTypes = \x -> Just (specialTypes, fmap (const ()) x)}))
             is)
        bindGroups
      {-trace ("Compiled classes: " ++ show env) (return ())-}
      typeClassEnv' <-
        catch
          (evalSupplyT
             (fmap
                M.fromList
                (mapM
                   (\(name, cls) -> do
                      is <-
                        mapM
                          (\inst -> do
                             ms <-
                               mapM
                                 (\(nam, alt) ->
                                    fmap
                                      (nam, )
                                      (resolveAlt typeClassEnv specialTypes alt))
                                 (M.toList
                                    (dictionaryMethods (instanceDictionary inst)))
                             pure
                               inst
                               { instanceDictionary =
                                   (instanceDictionary inst)
                                   {dictionaryMethods = M.fromList ms}
                               })
                          (classInstances cls)
                      pure (name, cls {classInstances = is}))
                   (M.toList typeClassEnv)))
             supplies)
          (\e ->
             liftIO
               (do putStrLn (displayResolveException specialTypes e)
                   exitFailure))
      bindGroups' <-
        catch
          (evalSupplyT
             (mapM (resolveBindGroup typeClassEnv' specialTypes) bindGroups)
             supplies)
          (\e ->
             liftIO
               (do putStrLn (displayResolveException specialTypes e)
                   exitFailure))
      putStrLn "-- Source with instance dictionaries inserted:"
      mapM_
        (\(BindGroup es is) -> do
           mapM_
             (putStrLn .
              printExplicitlyTypedBinding
                defaultPrint {printDictionaries = True}
                specialTypes)
             es
           mapM_
             (mapM_
                (putStrLn .
                 printImplicitlyTypedBinding
                   defaultPrint {printDictionaries = True}))
             is)
        bindGroups'
      case i of
        Nothing -> return ()
        Just i' -> do
          putStrLn "-- Stepping ..."
          catch
            (do e0 <- lookupNameByString i' bindGroups'
                evalSupplyT
                  (fix
                     (\loopy lastString e -> do
                        e' <-
                          expandSeq1
                            typeClassEnv'
                            specialSigs
                            signatures
                            e
                            bindGroups'
                        let string = printExpression (defaultPrint) e
                        when
                          (string/=lastString && (True || cleanExpression e))
                          (liftIO (putStrLn string))
                        if fmap (const ()) e' /= fmap (const ()) e
                          then do
                            renameExpression specials subs types e' >>= loopy string
                          else pure ())
                     ""
                     e0)
                  supplies)
            (\e ->
               liftIO
                 (do putStrLn (displayStepperException specialTypes e)
                     exitFailure))

--------------------------------------------------------------------------------
-- Clean expressions

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

displayResolveException :: SpecialTypes Name -> ResolveException -> String
displayResolveException specialTypes =
  \case
    NoInstanceFor p -> "No instance for " ++ printPredicate defaultPrint specialTypes p

displayStepperException :: a -> StepException -> String
displayStepperException _ =
  \case
    CouldntFindName n -> "Not in scope: " ++ curlyQuotes (printit defaultPrint n)
    CouldntFindNameByString n ->
      "The starter variable isn't defined: " ++
      curlyQuotes n ++
      "\nPlease define a variable called " ++ curlyQuotes n
    TypeAtValueScope k -> "Type at value scope: " ++ show k

displayInferException :: SpecialTypes Name -> InferException -> [Char]
displayInferException specialTypes =
  \case
    ExplicitTypeMismatch sc1 sc2 ->
      "The type of a definition doesn't match its explicit type:\n\n  " ++
     printScheme defaultPrint specialTypes sc1 ++ "\n\nand\n\n  " ++

     printScheme defaultPrint specialTypes sc2 ++ "\n\n" ++
       show sc1 ++ "\n" ++ show sc2
    NotInScope scope name ->
      "Not in scope " ++
      curlyQuotes (printit defaultPrint name) ++
      "\n" ++
      "Nearest names in scope:\n\n" ++
      intercalate
        ", "
        (map
           curlyQuotes
           (take
              5
              (sortBy
                 (comparing (editDistance (printit defaultPrint name)))
                 (map (printTypeSignature defaultPrint specialTypes) scope))))
    TypeMismatch t1 t2 ->
      "Couldn't match type " ++
      curlyQuotes (printType defaultPrint specialTypes t1) ++
      "\n" ++
      "against inferred type " ++ curlyQuotes (printType defaultPrint specialTypes t2)
    OccursCheckFails ->
      "Infinite type (occurs check failed). \nYou \
                        \probably have a self-referential value!"
    AmbiguousInstance ambiguities ->
      "Couldn't infer which instances to use for\n" ++
      unlines
        (map
           (\(Ambiguity _ ps) ->
              intercalate ", " (map (printPredicate defaultPrint specialTypes) ps))
           ambiguities)
    e -> show e

displayRenamerException :: SpecialTypes Name -> RenamerException -> [Char]
displayRenamerException specialTypes =
  wrap (\case
          IdentifierNotInVarScope scope name ->
            "Not in variable scope " ++
            curlyQuotes (printit defaultPrint name) ++
            "\n" ++
            "Nearest names in scope:\n\n" ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printit defaultPrint name)))
                       (map (printit defaultPrint) (M.elems scope)))))
          IdentifierNotInConScope scope name ->
            "Not in constructors scope " ++
            curlyQuotes (printit defaultPrint name) ++
            "\n" ++
            "Nearest names in scope:\n\n" ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printit defaultPrint name)))
                       (map (printit defaultPrint) (M.elems scope)))))
          KindTooManyArgs ty k ty2 ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes ty ++ " :: " ++ printKind k) ++
            " has an unexpected additional argument, " ++
            curlyQuotes (printType defaultPrint specialTypes ty2)
          ConstructorFieldKind cons typ kind ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes typ ++ " :: " ++ printKind kind) ++
            " is used in a field in the " ++
            curlyQuotes (printit defaultPrint cons) ++
            " constructor, but all fields \
            \should have types of kind " ++
            curlyQuotes (printKind StarKind)
          KindArgMismatch t1 k1 t2 k2 ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes t1 ++ " :: " ++ printKind k1) ++
            " has been given an argument of the wrong kind " ++
            curlyQuotes (printType defaultPrint specialTypes t2 ++ " :: " ++ printKind k2)
          TypeNotInScope types i ->
            "Unknown type " ++
            curlyQuotes (printIdentifier defaultPrint i) ++
            "\n" ++
            "Closest names in scope are: " ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printIdentifier defaultPrint i)))
                       (map (printTypeConstructor defaultPrint) types))))
          UnknownTypeVariable types i ->
            "Unknown type variable " ++
            curlyQuotes (printIdentifier defaultPrint i) ++
            "\n" ++
            "Type variables in scope are: " ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (sortBy
                    (comparing (editDistance (printIdentifier defaultPrint i)))
                    (map (printTypeVariable defaultPrint) types)))
          e -> show e)
  where wrap f e = (f e)-- ++ "\n(" ++ show e ++ ")"

editDistance :: [Char] -> [Char] -> Int
editDistance = on (levenshteinDistance defaultEditCosts) (map toLower)

runTypeChecker
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => [Decl UnkindedType Identifier Location]
  -> m ((SpecialSigs Name, SpecialTypes Name, [BindGroup Type Name (TypeSignature Type Name Location)], [TypeSignature Type Name Name], Map Identifier Name, Map Name (Class Type Name (TypeSignature Type Name Location)), [DataType Type Name]), [Int])
runTypeChecker decls =
  let bindings =
        mapMaybe
          (\case
             BindGroupDecl d -> Just d
             _ -> Nothing)
          decls
      classes =
        mapMaybe
          (\case
             ClassDecl d -> Just d
             _ -> Nothing)
          decls
      instances =
        mapMaybe
          (\case
             InstanceDecl d -> Just d
             _ -> Nothing)
          decls
      types =
        mapMaybe
          (\case
             DataDecl d -> Just d
             _ -> Nothing)
          decls
  in runSupplyT
       (do (specialTypes, specialSigs, signatures0, env0 ) <- setupEnv mempty
           let specials = Specials specialSigs specialTypes
           liftIO (putStrLn "-- Renaming types, classes and instances ...")
           (typeClasses, signatures, subs, dataTypes) <-
             catch
               (do dataTypes <- renameDataTypes specials types
                   consSigs <-
                     fmap
                       concat
                       (mapM (dataTypeSignatures specialTypes) dataTypes)
                   typeClasses0 <-
                     mapM
                       (\c -> do
                          renamed <- renameClass specials mempty dataTypes c
                          pure (className c, renamed))
                       classes
                   let typeClasses = map snd typeClasses0
                   methodSigs <- fmap concat (mapM classSignatures typeClasses)
                   let signatures = signatures0 <> consSigs <> methodSigs
                       subs =
                         M.fromList
                           (mapMaybe
                              (\(TypeSignature name _) ->
                                 case name of
                                   ValueName _ ident -> Just (Identifier ident, name)
                                   ConstructorName _ ident ->
                                     pure (Identifier ident, name)
                                   MethodName _ ident ->
                                     pure (Identifier ident, name)
                                   _ -> Nothing)
                              signatures) <>
                         M.fromList (map (second className) typeClasses0)
                   allInstances <-
                     mapM
                       (renameInstance specials subs dataTypes typeClasses)
                       instances
                   pure
                     ( map
                         (\typeClass ->
                            typeClass
                            { classInstances =
                                filter
                                  ((== className typeClass) . instanceClassName)
                                  allInstances
                            })
                         typeClasses
                     , signatures
                     , subs
                     , dataTypes))
               (\e ->
                  liftIO
                    (do putStrLn (displayRenamerException specialTypes e)
                        exitFailure))
           liftIO (putStrLn "-- Signatures in scope:")
           liftIO
             (mapM_ (putStrLn . printTypeSignature defaultPrint specialTypes) signatures)
           liftIO (putStrLn "-- Renaming variable/function declarations ...")
           (renamedBindings, subs') <-
             catch
               (renameBindGroups specials subs dataTypes bindings)
               (\e ->
                  liftIO
                    (do putStrLn (displayRenamerException specialTypes e)
                        exitFailure))

           env <-
             lift
               (foldM
                  (\e0 typeClass ->
                     addClass
                       typeClass
                       e0 >>= \e ->
                       foldM
                         (\e1 i
                           -> do addInstance i e1)
                         e
                         (classInstances typeClass))
                  env0
                  typeClasses)
           liftIO (putStrLn "-- Inferring types ...")
           (bindGroups, env') <-
             lift
               (catch
                  (typeCheckModule env signatures specialTypes renamedBindings)
                  (\e ->
                     liftIO
                       (do putStrLn (displayInferException specialTypes e)
                           exitFailure)))
           return
             (specialSigs, specialTypes, bindGroups, signatures, subs', env', dataTypes))
       [0 ..]

classSignatures :: MonadThrow m => Class Type Name l -> m [TypeSignature Type Name Name]
classSignatures cls =
  mapM
    (\(name, scheme) ->
       TypeSignature <$> pure name <*> classMethodScheme cls scheme)
    (M.toList (classMethods cls))

dataTypeSignatures
  :: Monad m
  => SpecialTypes Name -> DataType Type Name -> m [TypeSignature Type Name Name]
dataTypeSignatures specialTypes dt@(DataType _ vs cs) = mapM construct cs
  where
    construct (DataTypeConstructor cname fs) =
      pure
        (TypeSignature
           cname
           (Forall
              vs
              (Qualified
                 []
                 (foldr
                    makeArrow
                    (foldl
                       ApplicationType
                       (dataTypeConstructor dt)
                       (map VariableType vs))
                    fs))))
      where
        makeArrow :: Type Name -> Type Name -> Type Name
        a `makeArrow` b =
          ApplicationType
            (ApplicationType
               (ConstructorType (specialTypesFunction specialTypes))
               a)
            b

-- | Setup the class environment.
setupEnv
  :: (MonadThrow m)
  => Map Name (Class Type Name Location)
  -> SupplyT Int m (SpecialTypes Name, SpecialSigs Name, [TypeSignature Type Name Name], Map Name (Class Type Name Location))
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
      [( "times"
       , Alternative
           (Location 0 0 0 0)
           []
           (VariableExpression
              (Location 0 0 0 0)
              (PrimopName PrimopIntegerTimes)))
      ,( "plus"
       , Alternative
           (Location 0 0 0 0)
           []
           (VariableExpression
              (Location 0 0 0 0)
              (PrimopName PrimopIntegerPlus)))]
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
    in lift (update env)
  pure (specialTypes, specialSigs, signatures, env')

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

makeClass
  :: MonadSupply Int m
  => Identifier
  -> [TypeVariable Name]
  -> [(Name, Scheme t Name)]
  -> m (Class t Name l)
makeClass name vars methods = do
  name' <- supplyClassName name
  pure
    (Class
     { className = name'
     , classTypeVariables = vars
     , classInstances = []
     , classMethods = M.fromList methods
     , classSuperclasses = mempty
     })

makeInst
  :: MonadSupply Int m
  => Specials Name
  -> Predicate Type Name
  -> [(String, Alternative Type Name l)]
  -> m (Instance Type Name l)
makeInst specials pred methods = do
  name <- supplyDictName (predicateToDict specials pred)
  methods' <-
    mapM
      (\(key, alt) -> do
         key' <- supplyMethodName (Identifier key)
         pure (key', alt))
      methods
  pure (Instance (Qualified [] pred) (Dictionary name (M.fromList methods')))
