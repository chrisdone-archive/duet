
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
import           Duet.Tokenizer
import           Duet.Types
import           System.Environment
import           System.Exit
import           Text.EditDistance

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, i] -> do
      text <- T.readFile file
      compileStepText "<interactive>" i text
    _ -> error "usage: duet <file>"

compileStepText :: String -> String -> Text -> IO ()
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
      ((specialSigs, specialTypes, bindGroups, signatures, subs, typeClassEnv), supplies) <-
        runTypeChecker decls
      putStrLn "-- Type-checked bindings:"
      mapM_
        (\(BindGroup _ is) ->
           mapM_
             (mapM_ (putStrLn . printImplicitlyTypedBinding (defaultPrint)))
             is)
        bindGroups
      putStrLn "-- With type-annotations:"
      mapM_
        (\(BindGroup _ is) ->
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
        (\(BindGroup _ is) ->
           mapM_
             (mapM_
                (putStrLn .
                 printImplicitlyTypedBinding
                   defaultPrint {printDictionaries = True}))
             is)
        bindGroups'
      putStrLn "-- Stepping ..."
      catch
        (do e0 <- lookupNameByString i bindGroups'
            evalSupplyT
              (fix
                 (\loopy e -> do
                    when
                      (True || cleanExpression e)
                      (liftIO (putStrLn (printExpression (defaultPrint) e)))
                    e' <-
                      expandSeq1
                        typeClassEnv'
                        specialSigs
                        signatures
                        e
                        bindGroups
                        subs
                    if fmap (const ()) e' /= fmap (const ()) e
                      then do
                        renameExpression subs e' >>= loopy
                      else pure ())
                 e0)
              supplies)
        (\e ->
           liftIO
             (do putStrLn (displayStepperException specialTypes e)
                 exitFailure))

--------------------------------------------------------------------------------
-- Clean expressions

-- | Filter out expressions with intermediate case, if and immediately-applied lambdas.
cleanExpression :: Expression i l -> Bool
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
  => [Decl ParsedType Identifier Location]
  -> m ((SpecialSigs Name, SpecialTypes Name, [BindGroup Name (TypeSignature Name Location)], [TypeSignature Name Name], Map Identifier Name, Map Name (Class Type Name (TypeSignature Name Location))), [Int])
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
       (do specialTypes <- defaultSpecialTypes
           (specialSigs, signatures0) <- builtInSignatures specialTypes
           liftIO (putStrLn "-- Renaming types, classes and instances ...")
           (typeClasses, signatures, subs) <-
             catch
               (do dataTypes <- renameDataTypes specialTypes types
                   consSigs <-
                     fmap
                       concat
                       (mapM (dataTypeSignatures specialTypes) dataTypes)
                   typeClasses0 <-
                     mapM
                       (\c -> do
                          renamed <- renameClass specialTypes mempty dataTypes c
                          pure (className c, renamed))
                       classes
                   let typeClasses = map snd typeClasses0
                   methodSigs <- fmap concat (mapM classSignatures typeClasses)
                   let signatures = signatures0 <> consSigs <> methodSigs
                       subs =
                         M.fromList
                           (map
                              (\(TypeSignature name _) ->
                                 case name of
                                   ValueName _ ident -> (Identifier ident, name)
                                   ConstructorName _ ident ->
                                     (Identifier ident, name)
                                   MethodName _ ident ->
                                     (Identifier ident, name))
                              signatures) <>
                         M.fromList (map (second className) typeClasses0)
                   allInstances <-
                     mapM
                       (renameInstance specialTypes subs dataTypes typeClasses)
                       instances
                   {-trace ("Instances: " ++ show allInstances) (return ())-}
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
                     , subs))
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
               (renameBindGroups subs bindings)
               (\e ->
                  liftIO
                    (do putStrLn (displayRenamerException specialTypes e)
                        exitFailure))
           env0 <- setupEnv specialTypes mempty
           env <-
             lift
               (foldM
                  (\e0 typeClass ->
                     addClass
                       (className typeClass)
                       (classTypeVariables typeClass)
                       (classSuperclasses typeClass)
                       (classMethods typeClass)
                       e0 >>= \e ->
                       foldM
                         (\e1 i@(Instance (Qualified ps p) dict)
                               {-liftIO (putStrLn ("Add instance: " ++ show i))-}
                           -> do addInstance ps p dict e1)
                         e
                         (classInstances typeClass))
                  env0
                  typeClasses)
           -- liftIO (putStrLn "-- Type class environment:")
           -- liftIO (print env)
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
             (specialSigs, specialTypes, bindGroups, signatures, subs', env'))
       [0 ..]

-- | Built-in pre-defined functions.
builtInSignatures
  :: MonadThrow m
  => SpecialTypes Name -> SupplyT Int m (SpecialSigs Name, [TypeSignature Name Name])
builtInSignatures specialTypes = do
  the_show <- supplyValueName ("show" :: Identifier)
  sigs <- dataTypeSignatures specialTypes (specialTypesBool specialTypes)
  the_True <- getSig "True" sigs
  the_False <- getSig "False" sigs
  return
    ( SpecialSigs
      { specialSigsTrue = the_True
      , specialSigsFalse = the_False
      , specialSigsShow = the_show
      }
    , [ TypeSignature
          the_show
          (Forall
             [StarKind]
             (Qualified
                [IsIn (specialTypesShow specialTypes) [(GenericType 0)]]
                (GenericType 0 --> ConstructorType (specialTypesString specialTypes))))
      ] ++
      sigs)
  where
    getSig ident sigs =
      case listToMaybe
             (mapMaybe
                (\case
                   (TypeSignature n@(ValueName _ i) _)
                     | i == ident -> Just n
                   (TypeSignature n@(ConstructorName _ i) _)
                     | i == ident -> Just n
                   _ -> Nothing)
                sigs) of
        Nothing -> throwM (BuiltinNotDefined ident)
        Just sig -> pure sig

    (-->) :: Type Name -> Type Name -> Type Name
    a --> b =
      ApplicationType (ApplicationType (ConstructorType(specialTypesFunction specialTypes)) a) b

classSignatures :: MonadThrow m => Class Type Name l -> m [TypeSignature Name Name]
classSignatures cls =
  mapM
    (\(name, (methodVars, ty)) ->
       let gens = zip methodVars [GenericType i | i <- [0 ..]]
       in do ty' <- genify gens ty
             headVars <- mapM (genify gens . VariableType) (classTypeVariables cls)
             pure
               (TypeSignature
                  name
                  (Forall
                     (map typeVariableKind methodVars)
                     (Qualified [IsIn (className cls) headVars] ty'))))
    (M.toList (classMethods cls))

genify :: MonadThrow m => [(TypeVariable Name, Type Name)] -> Type Name -> m (Type Name)
genify table =
  \case
    VariableType tyvar ->
      case lookup tyvar table of
        Nothing -> throwM (InvalidMethodTypeVariable (map fst table) tyvar)
        Just v -> pure v
    ApplicationType f x -> do
      f' <- genify table f
      x' <- genify table x
      pure (ApplicationType f' x')
    x -> pure x

dataTypeSignatures
  :: Monad m
  => SpecialTypes Name -> DataType Type Name -> m [TypeSignature Name Name]
dataTypeSignatures specialTypes dt@(DataType _ vs cs) = mapM construct cs
  where
    construct (DataTypeConstructor cname fs) =
      pure
        (TypeSignature
           cname
           (let varsGens = map (second GenericType) (zip vs [0 ..])
            in Forall
                 (map typeVariableKind vs)
                 (Qualified
                    []
                    (foldr
                       makeArrow
                       (foldl
                          ApplicationType
                          (dataTypeConstructor dt)
                          (map snd varsGens))
                       (map (varsToGens varsGens) fs)))))
      where
        varsToGens :: [(TypeVariable Name, Type Name)] -> Type Name -> Type Name
        varsToGens varsGens = go
          where
            go =
              \case
                v@(VariableType tyvar) ->
                  case lookup tyvar varsGens of
                    Just gen -> gen
                    Nothing -> v
                ApplicationType t1 t2 -> ApplicationType (go t1) (go t2)
                g@GenericType {} -> g
                c@ConstructorType {} -> c
        makeArrow :: Type Name -> Type Name -> Type Name
        a `makeArrow` b =
          ApplicationType
            (ApplicationType (ConstructorType(specialTypesFunction specialTypes)) a)
            b

-- | Setup the class environment.
setupEnv
  :: MonadThrow m
  => SpecialTypes Name
  -> Map Name (Class Type Name l)
  -> SupplyT Int m (Map Name (Class Type Name l))
setupEnv specialTypes env = do
  show_a <- supplyTypeName "a"
  showInt <- supplyDictName "Show Int"
  showRational <- supplyDictName "Show Rational"
  showChar' <- supplyDictName "Show Char"
  let update =
        addClass theShow [TypeVariable show_a StarKind] [] mempty >=>
        addInstance
          []
          (IsIn theShow [ConstructorType(specialTypesInteger specialTypes)])
          (Dictionary showInt mempty) >=>
        addInstance
          []
          (IsIn theShow [ConstructorType(specialTypesRational specialTypes)])
          (Dictionary showRational mempty) >=>
        addInstance
          []
          (IsIn theShow [ConstructorType(specialTypesChar specialTypes)])
          (Dictionary showChar' mempty)
  lift (update env)
  where
    theShow = specialTypesShow specialTypes

--------------------------------------------------------------------------------
-- Built-in types

-- | Special types that Haskell uses for pattern matching and literals.
defaultSpecialTypes :: Monad m => SupplyT Int m (SpecialTypes Name)
defaultSpecialTypes = do
  boolDataType <-
    do name <- supplyTypeName "Bool"
       true <- supplyConstructorName "True"
       false <- supplyConstructorName "False"
       pure
         (DataType
            name
            []
            [DataTypeConstructor true [], DataTypeConstructor false []])
  theArrow <- supplyTypeName "(->)"
  theChar <- supplyTypeName "Char"
  theString <- supplyTypeName "String"
  theInteger <- supplyTypeName "Integer"
  theRational <- supplyTypeName "Rational"
  theShow <- supplyTypeName "Show"
  return
    (SpecialTypes
     { specialTypesBool = boolDataType
     , specialTypesChar = (TypeConstructor theChar StarKind)
     , specialTypesString = (TypeConstructor theString StarKind)
     , specialTypesFunction =
         (TypeConstructor
            theArrow
            (FunctionKind StarKind (FunctionKind StarKind StarKind)))
     , specialTypesInteger =
         (TypeConstructor theInteger StarKind)
     , specialTypesRational =
         (TypeConstructor theRational StarKind)
     , specialTypesShow = theShow
     })
