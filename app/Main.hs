{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
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
        Right bindings -> do
          putStrLn "-- Type checking ..."
          (specialSigs, specialTypes, bindGroups) <- runTypeChecker bindings
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
          e0 <- lookupNameByString i bindGroups
          fix
            (\loop e -> do
               e' <- expand specialSigs e bindGroups
               putStrLn (printExpression (const Nothing) e)
               if e' /= e
                 then loop e'
                 else pure ())
            e0
    _ -> error "usage: duet <file>"

displayInferException :: SpecialTypes Name -> InferException -> [Char]
displayInferException specialTypes =
  \case
    NotInScope scope name ->
      "Not in scope " ++
      curlyQuotes (printit name) ++
      "\n" ++
      "Current scope:\n\n" ++
      unlines (map (printTypeSignature specialTypes) scope)
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
    IdentifierNotInScope scope name ->
      "Not in scope " ++ curlyQuotes (printit name) ++ "\n" ++
      "Current scope:\n\n" ++ unlines (map printit (M.keys scope))

runTypeChecker
  :: (MonadThrow m, MonadCatch m, MonadIO m)
  => [BindGroup Identifier l]
  -> m (SpecialSigs Name, SpecialTypes Name, [BindGroup Name (TypeSignature Name l)])
runTypeChecker bindings =
  evalSupplyT
    (do specialTypes <- defaultSpecialTypes
        theShow <- supplyTypeName "Show"
        (specialSigs, signatures) <- builtInSignatures theShow specialTypes
        let signatureSubs =
              M.fromList
                (map
                   (\(TypeSignature name@(ValueName _ ident) _) ->
                      (Identifier ident, name))
                   signatures)
        (renamedBindings, _) <-
          catch
            (renameBindGroups signatureSubs bindings)
            (\e ->
               liftIO
                 (do putStrLn (displayRenamerException specialTypes e)
                     exitFailure))
        env <- setupEnv theShow specialTypes mempty
        bindGroups <-
          lift
            (catch
               (typeCheckModule env signatures specialTypes renamedBindings)
               (\e ->
                  liftIO
                    (do putStrLn (displayInferException specialTypes e)
                        exitFailure)))
        return (specialSigs, specialTypes, bindGroups))
    [0 ..]

-- | Built-in pre-defined functions.
builtInSignatures
  :: Monad m
  => Name -> SpecialTypes Name -> SupplyT Int m (SpecialSigs Name, [TypeSignature Name Name])
builtInSignatures theShow specialTypes = do
  the_show <- supplyValueName "show"
  the_True <- supplyValueName "True"
  the_False <- supplyValueName "False"
  return
    ( SpecialSigs {specialSigsTrue = the_True, specialSigsFalse = the_False}
    , [ TypeSignature
          the_show
          (Forall
             [StarKind]
             (Qualified
                [IsIn theShow [(GenericType 0)]]
                (GenericType 0 --> specialTypesString specialTypes)))
      , TypeSignature
          the_True
          (Forall [] (Qualified [] (specialTypesBool specialTypes)))
      , TypeSignature
          the_False
          (Forall [] (Qualified [] (specialTypesBool specialTypes)))
      ])
  where
    (-->) :: Type Name -> Type Name -> Type Name
    a --> b =
      ApplicationType (ApplicationType (specialTypesFunction specialTypes) a) b

-- | Setup the class environment.
setupEnv
  :: MonadThrow m
  => Name
  -> SpecialTypes Name
  -> ClassEnvironment Name
  -> SupplyT Int m (ClassEnvironment Name)
setupEnv theShow specialTypes env =
  do theNum <- supplyTypeName "Num"
     num_a <- supplyTypeName "a"
     show_a <- supplyTypeName "a"
     let update = addClass theNum [TypeVariable num_a StarKind] [] >=>
                  addInstance [] (IsIn theNum [specialTypesInteger specialTypes]) >=>
                  addClass theShow [TypeVariable show_a StarKind] [] >=>
                  addInstance [] (IsIn theShow [specialTypesChar specialTypes]) >=>
                  addInstance [] (IsIn theShow [specialTypesInteger specialTypes])
     lift (update env)

--------------------------------------------------------------------------------
-- Built-in types

-- | Special types that Haskell uses for pattern matching and literals.
defaultSpecialTypes :: Monad m => SupplyT Int m (SpecialTypes Name)
defaultSpecialTypes = do
  theBool <- supplyTypeName "Bool"
  theArrow <- supplyTypeName "(->)"
  theChar <- supplyTypeName "Char"
  theString <- supplyTypeName "String"
  theInteger <- supplyTypeName "Integer"
  theNum <- supplyTypeName "Num"
  theFractional <- supplyTypeName "Fractional"
  return
    (SpecialTypes
     { specialTypesBool = ConstructorType (TypeConstructor theBool StarKind)
     , specialTypesChar = ConstructorType (TypeConstructor theChar StarKind)
     , specialTypesString = ConstructorType (TypeConstructor theString StarKind)
     , specialTypesFunction =
         ConstructorType
           (TypeConstructor
              theArrow
              (FunctionKind StarKind (FunctionKind StarKind StarKind)))
     , specialTypesInteger =
         ConstructorType (TypeConstructor theInteger StarKind)
     , specialTypesNum = theNum
     , specialTypesFractional = theFractional
     })
