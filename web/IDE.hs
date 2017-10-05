{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Parser
import           Duet.Printer
import           Duet.Types
import           Reflex.Dom

main :: IO ()
main =
  mainWidget
    (do astEv <- el "p" (expressionEditor Nothing)
        astDyn <- holdDyn Nothing (fmap Just (astEv))
        printedDyn <-
          mapDyn
            (maybe "No AST currently." (printExpression defaultPrint))
            astDyn
        el "p" (dynText printedDyn))

expressionEditor
  :: MonadWidget t m
  => Maybe (Expression UnkindedType Identifier Location)
  -> m (Event t (Expression UnkindedType Identifier Location))
expressionEditor mdef = do
  inputWidget <-
    textInput
      def
      { _textInputConfig_initialValue =
          maybe "" (printExpression defaultPrint) mdef
      }
  parseResultDyn <-
    foldDyn
      (const . Just . parseTextWith expParser "input" . T.pack)
      (fmap Right mdef)
      (_textInput_input inputWidget)
  widgetDyn <-
    mapDyn
      (\case
         Nothing -> pure (updated (constDyn Nothing))
         Just (Left e) -> do
           void (divClass "danger" (text (show e)))
           pure (updated (constDyn (Just (Left e))))
         Just (Right e) ->
           case e of
             VariableExpression {} -> do
               text (printExpression defaultPrint e)
               pure (updated (constDyn (Just (Right e))))
             ApplicationExpression l f x -> do
               void (text "(")
               fDyn <- expressionEditor (Just f) >>= holdDyn f
               xDyn <- expressionEditor (Just x) >>= holdDyn x
               void (text ")")
               appsDyn <- combineDyn (ApplicationExpression l) fDyn xDyn
               pure (fmapMaybe (Just . Just . Right) (updated appsDyn))
             _ -> do
               void
                 (divClass
                    "warning"
                    (text ("Unsupported node type: " <> show e)))
               pure (updated (constDyn (Just (Right e)))))
      parseResultDyn
  streamsEv <- dyn widgetDyn
  currentValuesEv <- switchPromptly never streamsEv
  pure
    (fmapMaybe
       (>>= either (const Nothing) Just)
       (leftmost [updated parseResultDyn, currentValuesEv]))
