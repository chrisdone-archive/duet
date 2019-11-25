{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Supply
import           Control.Monad.Writer
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import           Duet.Context
import           Duet.Errors
import           Duet.Infer
import           Duet.Parser
import           Duet.Printer
import           Duet.Renamer
import           Duet.Setup
import           Duet.Simple
import           Duet.Stepper
import           Duet.Types
import           Options.Applicative.Simple
import           System.IO

data Run = Run
  { runInputFile :: FilePath
  , runMainIs :: String
  , runConcise :: Bool
  , runNumbered :: Bool
  , runSteps :: Maybe Integer
  , runHideSteps :: Bool
  } deriving (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  ((), cmd) <-
    simpleOptions
      "1.0"
      "Duet interpreter"
      "This is the interpreter for the Duet mini-Haskell educational language"
      (pure ())
      (do addCommand "types" "Print types in scope" runTypesPrint (pure ())
          addCommand "classes" "Print types in scope" runClassesPrint (pure ())
          addCommand
            "run"
            "Run the given program source"
            runProgram
            (Run <$>
             strArgument
               (metavar "FILEPATH" <> help "The .hs file to interpret") <*>
             strOption
               (long "main" <> metavar "NAME" <> help "The main value to run" <>
                value "main") <*>
             flag False True (long "concise" <> help "Concise view") <*>
             flag False True (long "numbered" <> help "Number outputs") <*>
             optional
               (option
                  auto
                  (long "steps" <> short 'n' <> metavar "steps" <>
                   help "Maximum number of steps to run (default: unlimited)")) <*>
             flag
               False
               True
               (long "hide-steps" <> help "Do not print the steps to stdout")))
  cmd

runTypesPrint :: () -> IO ()
runTypesPrint _ = do
  builtins <- evalSupplyT (setupEnv mempty []) [1 ..]
  putStrLn
    (printDataType
       defaultPrint
       (builtinsSpecialTypes builtins)
       (specialTypesBool (builtinsSpecialTypes builtins)))
  when
    False
    (putStrLn
       (printTypeConstructorOpaque
          defaultPrint
          (specialTypesChar (builtinsSpecialTypes builtins))))
  putStrLn
    (printTypeConstructorOpaque
       defaultPrint
       (specialTypesString (builtinsSpecialTypes builtins)))
  putStrLn
    (printTypeConstructorOpaque
       defaultPrint
       (specialTypesInteger (builtinsSpecialTypes builtins)))
  putStrLn
    (printTypeConstructorOpaque
       defaultPrint
       (specialTypesRational (builtinsSpecialTypes builtins)))
  where
    printTypeConstructorOpaque p = ("data " ++) . printTypeConstructor p

runClassesPrint :: () -> IO ()
runClassesPrint _ = do
  builtins <- evalSupplyT (setupEnv mempty []) [1 ..]
  mapM_
    (putStrLn . (++ "\n") . printClass defaultPrint (builtinsSpecialTypes builtins))
    (M.elems (builtinsTypeClasses builtins))

runProgram :: Run -> IO ()
runProgram run@Run {..} = do
  catch
    (catch
       (runNoLoggingT
          (evalSupplyT
             (do decls <- liftIO (parseFile runInputFile)
                 (binds, ctx) <- createContext decls
                 things <-
                   execWriterT
                     (runStepperIO
                        run
                        runSteps
                        ctx
                        (fmap (fmap typeSignatureA) binds)
                        runMainIs)
                 pure things)
             [1 ..]))
       (putStrLn . displayContextException))
    (putStrLn . displayParseException)

-- | Run the substitution model on the code.
runStepperIO ::
     forall m. (MonadSupply Int m, MonadThrow m, MonadIO m)
  => Run
  -> Maybe Integer
  -> Context Type Name Location
  -> [BindGroup Type Name Location]
  -> String
  -> m ()
runStepperIO Run {..} maxSteps ctx bindGroups' i = do
  e0 <- lookupNameByString i bindGroups'
  loop 1 "" e0
  where
    loop :: Integer -> String -> Expression Type Name Location -> m ()
    loop count lastString e = do
      e' <- expandSeq1 ctx bindGroups' e
      let string = printExpression (defaultPrint) e
      when
        (string /= lastString && not runHideSteps)
        (if cleanExpression e || not runConcise
           then liftIO
                  (putStrLn
                     ((if runNumbered
                         then "[" ++ show count ++ "]\n"
                         else "") ++
                      printExpression defaultPrint e))
           else pure ())
      e'' <- pickUpIO e'
      if (fmap (const ()) e'' /= fmap (const ()) e) &&
         case maxSteps of
           Just top -> count < top
           Nothing -> True
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
