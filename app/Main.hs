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
import Options.Applicative.Simple

data Run = Run
  { runInputFile :: FilePath
  , runMainIs :: String
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
          strArgument
            (metavar "NAME" <> help "The main value to run" <> value "main") <*>
            argument auto
              (metavar "NAME" <> help "The main value to run" <> value 100)))
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
    Right steps -> mapM_ (putStrLn . (++ "\n") . printExpression defaultPrint) steps
