{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Monad
import qualified Data.Text.IO as T
import           Duet
import           Duet.Parser
import           Duet.Printer
import           System.Environment

main :: IO ()
main = do
  env <- setupEnv mempty
  args <- getArgs
  case args of
    [file] -> do
      text <- T.readFile file
      case parseText file text of
        Left e -> error (show e)
        Right bindings -> do
          bindGroups <-
            typeCheckModule env builtInSignatures defaultSpecialTypes bindings
          mapM_
           (\(BindGroup _ is) ->
              mapM_
                (mapM_
                   (putStrLn .
                    printImplicitlyTypedBinding
                      (\x -> Just (defaultSpecialTypes, fmap (const ()) x))))
                is)
           bindGroups
    _ -> error "usage: duet <file>"

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
