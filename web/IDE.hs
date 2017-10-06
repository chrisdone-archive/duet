{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Parser
import           Duet.Printer
import           Duet.Types
import           Reflex.Dom

--------------------------------------------------------------------------------
-- Main entry point

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

--------------------------------------------------------------------------------
-- Expression editing

expressionEditor
  :: MonadWidget t m
  => Maybe (Expression UnkindedType Identifier Location)
  -> m (Event t (Expression UnkindedType Identifier Location))
expressionEditor =
  someEditor
    (printExpression defaultPrint)
    (parseTextWith expParser "expression" . T.pack)
    renderExpression

renderExpression
  :: MonadWidget t m
  => Expression UnkindedType Identifier Location
  -> m (Event t (Maybe (Either SomeException (Expression UnkindedType Identifier Location))))
renderExpression e =
  case e of
    VariableExpression {} -> atomic
    LiteralExpression {} -> atomic
    ConstructorExpression {} -> atomic
    ConstantExpression {} -> atomic
    ApplicationExpression l f x -> do
      text "("
      fDyn <- child f
      xDyn <- child x
      text ")"
      appsDyn <- combineDyn (ApplicationExpression l) fDyn xDyn
      bubble appsDyn
    IfExpression l cond then' else' -> do
      text "if"
      condDyn <- child cond
      text "then"
      thenDyn <- child then'
      text "else"
      elseDyn <- child else'
      makeIfDyn <- combineDyn (IfExpression l) condDyn thenDyn
      ifDyn <- combineDyn ($) makeIfDyn elseDyn
      bubble ifDyn
    InfixExpression l left op right -> do
      leftDyn <- child left
      opDyn <- operatorEditor (Just op) >>= holdDyn op
      rightDyn <- child right
      makeOpDyn <- combineDyn (InfixExpression l) leftDyn opDyn
      opDyn <- combineDyn ($) makeOpDyn rightDyn
      bubble opDyn
    _ -> do
      divClass "warning" (text ("Unsupported node type: " <> show e))
      pure (updated (constDyn (Just (Right e))))
  where
    atomic = do
      text (printExpression defaultPrint e)
      pure (updated (constDyn (Just (Right e))))
    child v = expressionEditor (Just v) >>= holdDyn v
    bubble = pure . fmapMaybe (Just . Just . Right) . updated

--------------------------------------------------------------------------------
-- Operator editing

operatorEditor
  :: MonadWidget t m
  => Maybe (String, Expression UnkindedType Identifier Location)
  -> m (Event t (String, Expression UnkindedType Identifier Location))
operatorEditor =
  someEditor
    (\(string, _op) -> string)
    (\input -> parseTextWith operatorParser "operator" (" " <> T.pack input <> " "))
    (\(string, op) -> do
       text string
       pure (updated (constDyn (Just (Right (string, op))))))

--------------------------------------------------------------------------------
-- Editor combinators

someEditor
  :: MonadWidget t m
  => (node -> String)
  -> (String -> Either SomeException node)
  -> (node -> m (Event t (Maybe (Either SomeException node))))
  -> Maybe node
  -> m (Event t node)
someEditor printer parser renderer mdef = do
  inputWidget <-
    textInput
      def
      { _textInputConfig_initialValue =
          maybe "" printer mdef
      }
  parseResultDyn <-
    foldDyn
      (const . Just . parser)
      (fmap Right mdef)
      (_textInput_input inputWidget)
  widgetDyn <-
    mapDyn
      (\case
         Nothing -> pure (updated (constDyn Nothing))
         Just (Left e) -> do
           divClass "danger" (text (show e))
           pure (updated (constDyn (Just (Left e))))
         Just (Right e) ->
           renderer e)
      parseResultDyn
  streamsEv <- dyn widgetDyn
  currentValuesEv <- switchPromptly never streamsEv
  pure
    (fmapMaybe
       (>>= either (const Nothing) Just)
       (leftmost [updated parseResultDyn, currentValuesEv]))