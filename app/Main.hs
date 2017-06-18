{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Simple compiler and stepper.

module Main where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Duet.Context
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Resolver
import           Duet.Stepper
import           Duet.Supply
import           Duet.Types
import           System.Environment

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
      evalSupplyT
        (do (bindGroups, context) <- runTypeChecker decls
            let specials = contextSpecials context
            typeClassEnv' <-
              fmap
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
                                      (resolveAlt
                                         (contextTypeClasses context)
                                         (contextSpecialTypes context)
                                         alt))
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
                   (M.toList (contextTypeClasses context)))
            bindGroups' <-
              (mapM
                 (resolveBindGroup typeClassEnv' (contextSpecialTypes context))
                 bindGroups)
            case i of
              Nothing -> return ()
              Just i' -> do
                e0 <- lookupNameByString i' bindGroups'
                fix
                  (\loopy lastString e -> do
                     e' <-
                       expandSeq1
                         typeClassEnv'
                         (contextSpecialSigs context)
                         (contextSignatures context)
                         e
                         bindGroups'
                     let string = printExpression (defaultPrint) e
                     when
                       (string /= lastString && (True || cleanExpression e))
                       (liftIO (putStrLn string))
                     if fmap (const ()) e' /= fmap (const ()) e
                       then do
                         renameExpression
                           specials
                           (contextScope context)
                           (contextDataTypes context)
                           e' >>=
                           loopy string
                       else pure ())
                  ""
                  e0)
        [0 ..]

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

runTypeChecker
  :: (MonadThrow m, MonadCatch m, MonadSupply Int m)
  => [Decl UnkindedType Identifier Location]
  -> m ([BindGroup Type Name (TypeSignature Type Name Location)]
       ,Context Type Name Location)
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
  in do builtins <- setupEnv mempty
        let specials = builtinsSpecials builtins
        (typeClasses, signatures, subs, dataTypes) <-
          do dataTypes <- renameDataTypes specials types
             consSigs <-
               fmap
                 concat
                 (mapM
                    (dataTypeSignatures (builtinsSpecialTypes builtins))
                    dataTypes)
             typeClasses0 <-
               mapM
                 (\c -> do
                    renamed <- renameClass specials mempty dataTypes c
                    pure (className c, renamed))
                 classes
             let typeClasses = map snd typeClasses0
             methodSigs <- fmap concat (mapM classSignatures typeClasses)
             let signatures = builtinsSignatures builtins <> consSigs <> methodSigs
                 subs =
                   M.fromList
                     (mapMaybe
                        (\(TypeSignature name _) ->
                           case name of
                             ValueName _ ident -> Just (Identifier ident, name)
                             ConstructorName _ ident ->
                               pure (Identifier ident, name)
                             MethodName _ ident -> pure (Identifier ident, name)
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
               , dataTypes)
        (renamedBindings, subs') <-
          renameBindGroups specials subs dataTypes bindings
        env <-
          foldM
            (\e0 typeClass ->
               addClass typeClass e0 >>= \e ->
                 foldM
                   (\e1 i -> do addInstance i e1)
                   e
                   (classInstances typeClass))
            (builtinsTypeClasses builtins)
            typeClasses
        (bindGroups, env') <-
          typeCheckModule env signatures (builtinsSpecialTypes builtins) renamedBindings
        return
          ( bindGroups
          , Context
            { contextSpecialSigs = builtinsSpecialSigs builtins
            , contextSpecialTypes = builtinsSpecialTypes builtins
            , contextSignatures = signatures
            , contextScope = subs'
            , contextTypeClasses = env'
            , contextDataTypes = dataTypes
            })

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
