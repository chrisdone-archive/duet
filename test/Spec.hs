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
import Data.Bifunctor
import Duet.Infer
import Duet.Parser
import Duet.Simple
import Duet.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Compilation"
    (do it
          "Basic compile and run constant"
          (shouldBe
             (first
                (const ())
                (runNoLoggingT
                   ((evalSupplyT
                       (do decls <- parseText "test" "main = 1"
                           (binds, ctx) <- createContext decls
                           things <-
                             execWriterT
                               (runStepper
                                  100
                                  ctx
                                  (fmap (fmap typeSignatureA) binds)
                                  "main")
                           pure things)
                       [1 ..]))))
             (Right [LiteralExpression () (IntegerLiteral 1)]))
        it
          "Basic compile and run constant lambda"
          (shouldBe
             (first
                (const ())
                (runNoLoggingT
                   ((evalSupplyT
                       (do decls <- parseText "test" "main = (\\x -> x) 1"
                           (binds, ctx) <- createContext decls
                           things <-
                             execWriterT
                               (runStepper
                                  100
                                  ctx
                                  (fmap (fmap typeSignatureA) binds)
                                  "main")
                           pure things)
                       [1 ..]))))
             (Right
                [ ApplicationExpression
                    ()
                    (LambdaExpression
                       ()
                       (Alternative
                          { alternativeLabel = ()
                          , alternativePatterns =
                              [VariablePattern () (ValueName 49 "x")]
                          , alternativeExpression =
                              VariableExpression () (ValueName 49 "x")
                          }))
                    (LiteralExpression () (IntegerLiteral 1))
                , LiteralExpression () (IntegerLiteral 1)
                ]))
        it
          "Seq"
          (shouldBe
             (second
                last
                (first
                   (const ())
                   (runNoLoggingT
                      ((evalSupplyT
                          (do decls <-
                                parseText
                                  "test"
                                  "seq =\n\
                                 \  \\x y ->\n\
                                 \    case x of\n\
                                 \      !_ -> y\n\
                                 \loop = loop\n\
                                 \main = seq loop 1"
                              (binds, ctx) <- createContext decls
                              things <-
                                execWriterT
                                  (runStepper
                                     100
                                     ctx
                                     (fmap (fmap typeSignatureA) binds)
                                     "main")
                              pure things)
                          [1 ..])))))
             (Right
                (CaseExpression
                   ()
                   (VariableExpression () (ValueName 49 "loop"))
                   [ CaseAlt
                       { caseAltLabel = ()
                       , caseAltPattern = BangPattern (WildcardPattern () "_")
                       , caseAltExpression =
                           LiteralExpression () (IntegerLiteral 1)
                       }
                   ]))))
