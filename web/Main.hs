{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Control.Monad.Catch.Pure
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Stepper
import           Duet.Tokenizer
import           Duet.Types
import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  source <-
    Snappy.textbox
      snap
      (pure 20)
      (pure 20)
      (pure 500)
      (pure 250)
      (pure defaultSource)
  let result =
        fmap
          (\source ->
             case parseText "<input box>" (T.pack source) of
               Left e -> Left (show e)
               Right bindings ->
                 case runCatch
                        (do (specialSigs, specialTypes, bindGroups) <-
                              runTypeChecker bindings
                            e0 <- lookupNameByString "main" bindGroups
                            fix
                              (\loop e xs -> do
                                 e' <- expand specialSigs e bindGroups
                                 if e' /= e || length xs > 100
                                   then loop e' (e : xs)
                                   else pure (reverse (e : xs)))
                              e0
                              []) of
                   Left e -> Left (displayException e)
                   Right v -> Right v)
          (Snappy.eventToDynamic defaultSource (Snappy.textboxChange source))
  Snappy.textbox
    snap
    (pure 20)
    (pure 290)
    (pure 500)
    (pure 250)
    (fmap
       (\case
          Left err -> err
          Right steps -> unlines (map (printExpression (const Nothing)) steps))
       result)
  pure ()

--------------------------------------------------------------------------------
-- Renamer and inferer

data CheckerException
  = RenamerException (SpecialTypes Name) RenamerException
  | InferException (SpecialTypes Name) InferException
  deriving (Typeable, Show)
instance Exception CheckerException where
  displayException =
    \case
      RenamerException specialTypes e -> displayRenamerException specialTypes e
      InferException specialTypes e -> displayInferException specialTypes e

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
    e -> show e

displayRenamerException :: SpecialTypes Name -> RenamerException -> [Char]
displayRenamerException _specialTypes =
  \case
    IdentifierNotInScope scope name ->
      "Not in scope " ++ curlyQuotes (printit name) ++ "\n" ++
      "Current scope:\n\n" ++ unlines (map printit (M.keys scope))

runTypeChecker
  :: (MonadThrow m, MonadCatch m)
  => [BindGroup Identifier l]
  -> m (SpecialSigs Name, SpecialTypes Name, [BindGroup Name (TypeSignature Name l)])
runTypeChecker bindings =
  evalSupplyT
    (do specialTypes <- defaultSpecialTypes
        theShow <- supplyName "Show"
        (specialSigs, signatures) <- builtInSignatures theShow specialTypes
        let signatureSubs =
              M.fromList
                (map
                   (\(TypeSignature name@(NameFromSource _ ident) _) ->
                      (Identifier ident, name))
                   signatures)
        (renamedBindings, _) <-
          catch
            (renameBindGroups signatureSubs bindings)
            (\e ->
               throwM (RenamerException specialTypes e))
        env <- setupEnv theShow specialTypes mempty
        bindGroups <-
          lift
            (catch
               (typeCheckModule env signatures specialTypes renamedBindings)
               (\e ->
                  throwM (InferException specialTypes e)))
        return (specialSigs, specialTypes, bindGroups))
    [0 ..]

--------------------------------------------------------------------------------
-- Default environment

defaultSource :: String
defaultSource =
  unlines
    [ "compose = \\f g x -> f (g x)"
    , "id = \\x -> x"
    , "main = id (if id True"
    , "              then \"Yay!\""
    , "              else id id \"Nay!\")"
    ]

-- | Built-in pre-defined functions.
builtInSignatures
  :: Monad m
  => Name -> SpecialTypes Name -> SupplyT Int m (SpecialSigs Name, [TypeSignature Name Name])
builtInSignatures theShow specialTypes = do
  the_show <- supplyName "show"
  the_True <- supplyName "True"
  the_False <- supplyName "False"
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
  do theNum <- supplyName "Num"
     num_a <- supplyName "a"
     show_a <- supplyName "a"
     let update = addClass theNum [TypeVariable num_a StarKind] [] >=>
                  addInstance [] (IsIn theNum [specialTypesInteger specialTypes]) >=>
                  addClass theShow [TypeVariable show_a StarKind] [] >=>
                  addInstance [] (IsIn theShow [specialTypesChar specialTypes]) >=>
                  addInstance [] (IsIn theShow [specialTypesInteger specialTypes])
     lift (update env)

-- | Special types that Haskell uses for pattern matching and literals.
defaultSpecialTypes :: Monad m => SupplyT Int m (SpecialTypes Name)
defaultSpecialTypes = do
  theBool <- supplyName "Bool"
  theArrow <- supplyName "(->)"
  theChar <- supplyName "Char"
  theString <- supplyName "String"
  theInteger <- supplyName "Integer"
  theNum <- supplyName "Num"
  theFractional <- supplyName "Fractional"
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
