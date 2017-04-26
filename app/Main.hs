{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import qualified Data.Text.IO as T
import           Duet
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Stepper
import           Duet.Types
import           System.Environment

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
          (specialTypes, bindGroups) <-
            evalSupplyT
              (do specialTypes <- defaultSpecialTypes
                  theShow <- supplyName "Show"
                  signatures <- builtInSignatures theShow specialTypes
                  renamedBindings <- rename bindings
                  env <- setupEnv theShow specialTypes mempty
                  bindGroups <-
                    lift
                      (typeCheckModule
                         env
                         signatures
                         specialTypes
                         renamedBindings)
                  return (specialTypes, bindGroups))
              [0 ..]
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
               e' <- expand e bindGroups
               putStrLn (printExpression (const Nothing) e)
               if e' /= e
                 then loop e'
                 else pure ())
            e0
    _ -> error "usage: duet <file>"

-- | Built-in pre-defined functions.
builtInSignatures
  :: Monad m
  => Name -> SpecialTypes Name -> SupplyT Int m [TypeSignature Name Name]
builtInSignatures theShow specialTypes = do
  the_show <- supplyName "show"
  return [ TypeSignature
             the_show
             (Forall
                [StarKind]
                (Qualified
                   [IsIn theShow [(GenericType 0)]]
                   (GenericType 0 --> specialTypesString specialTypes)))
         ]
  where (-->) :: Type Name -> Type Name -> Type Name
        a --> b =
          ApplicationType
            (ApplicationType (specialTypesFunction specialTypes) a)
            b

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

--------------------------------------------------------------------------------
-- Built-in types

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
