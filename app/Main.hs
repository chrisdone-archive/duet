{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, i] -> do
      text <- T.readFile file
      case parseText file text of
        Left e -> error (show e)
        Right decls -> do
          putStrLn "-- Type checking ..."
          ((specialSigs, specialTypes, bindGroups, signatures, subs), supplies) <-
            runTypeChecker decls
          putStrLn "-- Source: "
          mapM_
            (\(BindGroup _ is) ->
               mapM_
                 (mapM_
                    (putStrLn .
                     printImplicitlyTypedBinding
                       (\x -> Just (specialTypes, fmap (const ()) x))))
                 is)
            bindGroups
          putStrLn "-- Stepping ..."
          catch
            (do e0 <- lookupNameByString i bindGroups
                evalSupplyT
                  (fix
                     (\loopy e -> do
                        when
                          (True || cleanExpression e)
                          (liftIO (putStrLn (printExpression (\x -> Just (specialTypes, fmap (const ()) x)) {-(const Nothing)-} e)))
                        e' <-
                          expandSeq1 specialSigs signatures e bindGroups subs
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
    _ -> error "usage: duet <file>"

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

displayStepperException :: a -> StepException -> String
displayStepperException _ =
  \case
    CouldntFindName n -> "Not in scope: " ++ curlyQuotes (printit n)
    CouldntFindNameByString n ->
      "The starter variable isn't defined: " ++
      curlyQuotes n ++
      "\nPlease define a variable called " ++ curlyQuotes n

displayInferException :: SpecialTypes Name -> InferException -> [Char]
displayInferException specialTypes =
  \case
    NotInScope scope name ->
      "Not in scope " ++
      curlyQuotes (printit name) ++
      "\n" ++
      "Current scope:\n\n" ++
      unlines (map (printTypeSignature specialTypes) scope)
    TypeMismatch t1 t2 ->
      "Couldn't match type " ++
      curlyQuotes (printType specialTypes t1) ++
      "\n" ++
      "against inferred type " ++ curlyQuotes (printType specialTypes t2)
    OccursCheckFails ->
      "Infinite type (occurs check failed). \nYou \
                        \probably have a self-referential value!"
    AmbiguousInstance ambiguities ->
      "Couldn't infer which instances to use for\n" ++
      unlines
        (map
           (\(Ambiguity t ps) ->
              intercalate ", " (map (printPredicate specialTypes) ps))
           ambiguities)
    e -> show e

displayRenamerException :: SpecialTypes Name -> RenamerException -> [Char]
displayRenamerException _specialTypes =
  \case
    IdentifierNotInVarScope scope name ->
      "Not in variable scope " ++ curlyQuotes (printit name) ++ "\n" ++
      "Current scope:\n\n" ++ unlines (map printit (M.elems scope))
    IdentifierNotInConScope scope name ->
      "Not in constructors scope " ++ curlyQuotes (printit name) ++ "\n" ++
      "Current scope:\n\n" ++ unlines (map printit (M.elems scope))

runTypeChecker
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => [Decl FieldType Identifier l]
  -> m ((SpecialSigs Name, SpecialTypes Name, [BindGroup Name (TypeSignature Name l)], [TypeSignature Name Name], Map Identifier Name), [Int])
runTypeChecker decls =
  let bindings =
        mapMaybe
          (\case
             BindGroupDecl d -> Just d
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
           sigs' <-
             renameDataTypes specialTypes types >>=
             mapM (dataTypeSignatures specialTypes)
           let signatures = signatures0 ++ concat sigs'
           liftIO
             (mapM_ (putStrLn . printTypeSignature specialTypes) (concat sigs'))
           let signatureSubs =
                 M.fromList
                   (map
                      (\(TypeSignature name _) ->
                         case name of
                           ValueName _ ident -> (Identifier ident, name)
                           ConstructorName _ ident -> (Identifier ident, name))
                      signatures)
           (renamedBindings, subs) <-
             catch
               (renameBindGroups signatureSubs bindings)
               (\e ->
                  liftIO
                    (do putStrLn (displayRenamerException specialTypes e)
                        exitFailure))
           env <- setupEnv specialTypes mempty
           bindGroups0 <-
             lift
               (catch
                  (typeCheckModule env signatures specialTypes renamedBindings)
                  (\e ->
                     liftIO
                       (do putStrLn (displayInferException specialTypes e)
                           exitFailure)))
           bindGroups <- mapM resolveBindGroup bindGroups0
           return (specialSigs, specialTypes, bindGroups, signatures, subs))
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
                (GenericType 0 --> specialTypesString specialTypes)))
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
      ApplicationType (ApplicationType (specialTypesFunction specialTypes) a) b

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
            (ApplicationType (specialTypesFunction specialTypes) a)
            b

-- | Setup the class environment.
setupEnv
  :: MonadThrow m
  => SpecialTypes Name
  -> ClassEnvironment Name
  -> SupplyT Int m (ClassEnvironment Name)
setupEnv specialTypes env = do
  theNum <- supplyTypeName "Num"
  num_a <- supplyTypeName "a"
  let update =
        addClass theNum [TypeVariable num_a StarKind] [] >=>
        addInstance [] (IsIn theNum [specialTypesInteger specialTypes]) >=>
        addInstance [] (IsIn theNum [specialTypesRational specialTypes])
  lift (update env)

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
     , specialTypesChar = ConstructorType (TypeConstructor theChar StarKind)
     , specialTypesString = ConstructorType (TypeConstructor theString StarKind)
     , specialTypesFunction =
         ConstructorType
           (TypeConstructor
              theArrow
              (FunctionKind StarKind (FunctionKind StarKind StarKind)))
     , specialTypesInteger =
         ConstructorType (TypeConstructor theInteger StarKind)
     , specialTypesRational =
         ConstructorType (TypeConstructor theRational StarKind)
     , specialTypesShow = theShow
     })
