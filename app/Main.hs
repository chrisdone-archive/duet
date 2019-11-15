{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

import Control.Monad.Logger
import Control.Monad.Supply
import Control.Monad.Writer
import Data.Semigroup ((<>))
import Duet.Infer
import Duet.Parser
import Duet.Printer
import Duet.Simple
import Duet.Stepper
import Options.Applicative.Simple

data Run = Run
  { runInputFile :: FilePath
  , runMainIs :: String
  , runConcise :: Bool
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
          option
            auto
            (long "steps" <> short 'n' <> metavar "steps" <>
             help "Maximum number of steps to run (default: 100)" <>
             value 100)))
  cmd

runProgram :: Run -> IO ()
runProgram Run {..} = do
  decls <- parseFile runInputFile
  case runNoLoggingT
         ((evalSupplyT
             (do (binds, ctx) <- createContext decls
                 things <-
                   execWriterT
                     (runStepper
                        runSteps
                        ctx
                        (fmap (fmap typeSignatureA) binds)
                        runMainIs)
                 pure things)
             [1 ..])) of
    Left err -> print err
    Right steps ->
      mapM_
        (\(step, expr) ->
           putStrLn
             ("[" ++ show step ++ "]\n" ++ printExpression defaultPrint expr))
        (zip
           [1 :: Integer ..]
           (filter
              (\expr ->
                 if runConcise
                   then cleanExpression expr
                   else True)
              steps))

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
