{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Simple compiler and stepper.

module Main where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Supply
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
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
import           Shared
import           System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (file:is) -> do
      text <- T.readFile file
      runStdoutLoggingT
        (filterLogger
           (\_ level -> level >= LevelInfo)
           (evalSupplyT
              (do (binds, context) <- createContext "<interactive>" text
                  maybe (return ()) (runStepper context binds) (listToMaybe is))
              [1 ..]))
    _ -> error "usage: duet <file>"

--------------------------------------------------------------------------------
-- Context setup

-- | Create a context of all renamed, checked and resolved code.
createContext
  :: (MonadSupply Int m, MonadThrow m, MonadLogger m)
  => String
  -> Text
  -> m ([BindGroup Type Name (TypeSignature Type Name Location)], Context Type Name Location)
createContext file text = do
  do decls <- parseText file text
     builtins <- setupEnv mempty
     let specials = builtinsSpecials builtins
     -- Renaming
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
     printDebugDicts builtins bindGroups
     -- Type class resolution
     resolvedTypeClasses <-
       resolveTypeClasses typeCheckedClasses (builtinsSpecialTypes builtins)
     resolvedBindGroups <-
       mapM
         (resolveBindGroup resolvedTypeClasses (builtinsSpecialTypes builtins))
         bindGroups
     printDebugTypeChecked builtins resolvedBindGroups
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
     pure (resolvedBindGroups, context)

--------------------------------------------------------------------------------
-- Debug info

printDebugTypeChecked
  :: (PrintableType t, Printable i, Foldable t1, MonadLogger m)
  => Builtins t2 i l -> t1 (BindGroup t i (TypeSignature Type i b)) -> m ()
printDebugTypeChecked builtins bindGroups = do
  $logDebug "\n--- Explicitly typed\n"
  mapM_
    (\(BindGroup es is) -> do
       (mapM_
          ($logDebug . T.pack .
           printExplicitlyTypedBinding
             defaultPrint
             { printTypes =
                 \x -> Just (builtinsSpecialTypes builtins, fmap (const ()) x)
             }
             (builtinsSpecialTypes builtins))
          es)
       mapM_
         (mapM_
            ($logDebug . T.pack .
             printImplicitlyTypedBinding
               defaultPrint
               { printTypes =
                   \x -> Just (builtinsSpecialTypes builtins, fmap (const ()) x)
               }))
         is)
    bindGroups

printDebugDicts
  :: (PrintableType t, Printable i, Foldable t1, MonadLogger m)
  => Builtins t2 i l -> t1 (BindGroup t i (TypeSignature Type i b)) -> m ()
printDebugDicts builtins bindGroups = do
  $logDebug "\n--- Explicitly typed\n"
  mapM_
    (\(BindGroup es is) -> do
       (mapM_
          ($logDebug . T.pack .
           printExplicitlyTypedBinding
             defaultPrint
             { printDictionaries = True
             }
             (builtinsSpecialTypes builtins))
          es)
       mapM_
         (mapM_
            ($logDebug . T.pack .
             printImplicitlyTypedBinding
               defaultPrint
               { printDictionaries = True
               }))
         is)
    bindGroups

--------------------------------------------------------------------------------
-- Stepper

-- | Run the substitution model on the code.
runStepper
  :: (MonadSupply Int m, MonadThrow m, MonadLogger m, MonadIO m)
  => Context Type Name Location
  -> [BindGroup Type Name (TypeSignature Type Name Location)]
  -> String
  -> m ()
runStepper context bindGroups' i = do
  $logDebug "\n-- Stepping ...\n"
  e0 <- lookupNameByString i bindGroups'
  fix
    (\loopy lastString e -> do
       e' <- expandSeq1 context bindGroups' e
       let string =
             printExpression defaultPrint e
       when
         (string /= lastString && (True || cleanExpression e))
         (do liftIO (putStrLn string)
             $logDebug (T.pack (show (fmap (const ()) e))))
       if fmap (const ()) e' /= fmap (const ()) e
         then do
           renameExpression
             (contextSpecials context)
             (contextScope context)
             (contextDataTypes context)
             e' >>=
             loopy string
         else pure ())
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
