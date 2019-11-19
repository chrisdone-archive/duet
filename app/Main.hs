{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Supply
import Control.Monad.Writer
import Data.Semigroup ((<>))
import Duet.Context
import Duet.Infer
import Duet.Parser
import Duet.Printer
import Duet.Renamer
import Duet.Simple
import Duet.Stepper
import Duet.Types
import Options.Applicative.Simple

data Run = Run
  { runInputFile :: FilePath
  , runMainIs :: String
  , runConcise :: Bool
  , runNumbered :: Bool
  , runSteps :: Int
  } deriving (Show)

main :: IO ()
main = do
  ((), cmd) <-
    simpleOptions
      "1.0"
      "Duet interpreter"
      "This is the interpreter for the Duet mini-Haskell educational language"
      (pure ())
      (addCommand
         "run"
         "Run the given program source"
         runProgram
         (Run <$>
          strArgument (metavar "FILEPATH" <> help "The .hs file to interpret") <*>
          strOption
            (long "main" <> metavar "NAME" <> help "The main value to run" <>
             value "main") <*>
          flag False True (long "concise" <> help "Concise view") <*>
          flag False True (long "numbered" <> help "Number outputs") <*>
          option
            auto
            (long "steps" <> short 'n' <> metavar "steps" <>
             help "Maximum number of steps to run (default: 100)" <>
             value 100)))
  cmd

runProgram :: Run -> IO ()
runProgram run@Run {..} = do
  decls <- parseFile runInputFile
  runNoLoggingT
    (evalSupplyT
       (do (binds, ctx) <- createContext decls
           things <-
             execWriterT
               (runStepperIO
                  run
                  runSteps
                  ctx
                  (fmap (fmap typeSignatureA) binds)
                  runMainIs)
           pure things)
       [1 ..])

-- | Run the substitution model on the code.
runStepperIO
  :: forall m. (MonadSupply Int m, MonadThrow m, MonadIO m)
  => Run -> Int
  -> Context Type Name Location
  -> [BindGroup Type Name Location]
  -> String
  -> m ()
runStepperIO Run {..} maxSteps ctx bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  loop 1 "" e0
  where
    loop :: Int -> String -> Expression Type Name Location -> m ()
    loop count lastString e = do
      e' <- expandSeq1 ctx bindGroups' e
      let string = printExpression (defaultPrint) e
      when
        (string /= lastString)
        (if cleanExpression e || not runConcise
           then liftIO
                  (putStrLn
                     ((if runNumbered
                         then "[" ++ show count ++ "]\n"
                         else "") ++
                      printExpression defaultPrint e))
           else pure ())
      e'' <- pickUpIO e'
      if (fmap (const ()) e'' /= fmap (const ()) e) && count < maxSteps
        then do
          newE <-
            renameExpression
              (contextSpecials ctx)
              (contextScope ctx)
              (contextDataTypes ctx)
              e''
          loop (count + 1) string newE
        else pure ()

pickUpIO :: MonadIO m => Expression t Name l -> m (Expression t Name l)
pickUpIO =
  \case
    ApplicationExpression _ (ApplicationExpression _ (ConstructorExpression _ (ConstructorName _ "PutStrLn")) (LiteralExpression _ (StringLiteral toBePrinted))) next -> do
      liftIO (putStrLn toBePrinted)
      pure next
    ApplicationExpression l (ConstructorExpression _ (ConstructorName _ "GetLine")) func -> do
      inputString <- liftIO getLine
      pure (ApplicationExpression l func (LiteralExpression l (StringLiteral inputString)))
    e -> pure e

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
