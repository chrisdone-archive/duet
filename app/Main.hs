{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Monad
import qualified Data.Text.IO as T
import           Duet
import           Duet.Parser
import           System.Environment
import           Text.Parsec

main :: IO ()
main = do
  env <- setupEnv mempty
  args <- getArgs
  case args of
    [file] -> do
      text <- T.readFile file
      case parse
             (sepBy (implicitlyTypedBindingParser <* optional newline) newline)
             file
             text of
        Left e -> error (show e)
        Right bindings -> do
          bindGroups <-
            typeCheckModule
              env
              builtInSignatures
              defaultSpecialTypes
              [BindGroup [] (map return bindings)]
          print bindGroups
    [] -> do
      text <- T.getContents
      case parse
             (sepBy (implicitlyTypedBindingParser <* optional newline) newline)
             "<interactive>"
             text of
        Left e -> error (show e)
        Right bindings -> do
          bindGroups <-
            typeCheckModule
              env
              builtInSignatures
              defaultSpecialTypes
              [BindGroup [] (map return bindings)]
          print bindGroups
    _ -> error "usage: duet <file> or pipe a module into duet"

builtInSignatures :: [TypeSignature Identifier]
builtInSignatures =
  [ TypeSignature
      "show"
      (Forall
         [StarKind]
         (Qualified
            [IsIn "Show" [(GenericType 0)]]
            (GenericType 0 --> stringType)))
  ]

setupEnv :: ClassEnvironment -> IO ClassEnvironment
setupEnv =
  addClass "Num" [TypeVariable "a" StarKind] [] >=>
  addInstance [] (IsIn "Num" [specialTypesInteger defaultSpecialTypes]) >=>
  addClass "Show" [TypeVariable "a" StarKind] [] >=>
  addInstance [] (IsIn "Show" [specialTypesChar defaultSpecialTypes]) >=>
  addInstance [] (IsIn "Show" [specialTypesInteger defaultSpecialTypes])

(-->) :: Type -> Type -> Type
a --> b =
  ApplicationType
    (ApplicationType (specialTypesFunction defaultSpecialTypes) a)
    b

--------------------------------------------------------------------------------
-- Built-in types

stringType :: Type
stringType = ConstructorType (TypeConstructor "String" StarKind)

-- | Special types that Haskell uses for pattern matching and literals.
defaultSpecialTypes :: SpecialTypes
defaultSpecialTypes =
  SpecialTypes
  { specialTypesBool = ConstructorType (TypeConstructor "Bool" StarKind)
  , specialTypesChar = ConstructorType (TypeConstructor "Char" StarKind)
  , specialTypesString = makeListType (specialTypesChar defaultSpecialTypes)
  , specialTypesFunction =
      ConstructorType
        (TypeConstructor
           "(->)"
           (FunctionKind StarKind (FunctionKind StarKind StarKind)))
  , specialTypesList = listType
  , specialTypesInteger = ConstructorType (TypeConstructor "Integer" StarKind)
  }
  where
    makeListType :: Type -> Type
    makeListType t = ApplicationType listType t
    listType :: Type
    listType =
      ConstructorType (TypeConstructor "[]" (FunctionKind StarKind StarKind))
