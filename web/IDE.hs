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
    (do astEv <- el "div" (runEditor expressionEditor Nothing)
        astDyn <- holdDyn Nothing (fmap Just astEv)
        printedDyn <-
          mapDyn
            (maybe "No AST currently." (printExpression defaultPrint))
            astDyn
        el "div" (dynText printedDyn))

--------------------------------------------------------------------------------
-- Expression editing

expressionEditor
  :: MonadWidget t m
  => Editor t m (Expression UnkindedType Identifier Location)
expressionEditor =
  Editor
  { editorPrinter = printExpression defaultPrint
  , editorParser = parseTextWith expParser "expression" . T.pack
  , editorRenderer = renderExpression
  }

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
      opDyn <- runEditor operatorEditor (Just op) >>= holdDyn op
      rightDyn <- child right
      makeOpDyn <- combineDyn (InfixExpression l) leftDyn opDyn
      opDyn <- combineDyn ($) makeOpDyn rightDyn
      bubble opDyn
    LambdaExpression l (Alternative l' params expr) -> do
      text "\\"
      paramsDyn <- runEditor parametersEditor (Just params) >>= holdDyn params
      text "->"
      exprDyn <- child expr
      lamDyn <-
        combineDyn
          (\ps e -> LambdaExpression l (Alternative l' ps e))
          paramsDyn
          exprDyn
      bubble lamDyn
    _ -> do
      divClass "warning" (text ("Unsupported node type: " <> show e))
      pure (updated (constDyn (Just (Right e))))
  where
    atomic = do
      text (printExpression defaultPrint e)
      pure (updated (constDyn (Just (Right e))))
    child v = runEditor expressionEditor (Just v) >>= holdDyn v

--------------------------------------------------------------------------------
-- Operator editing

operatorEditor
  :: MonadWidget t m
  => Editor t m (String, Expression UnkindedType Identifier Location)
operatorEditor =
  Editor
  { editorPrinter = \(string, _op) -> string
  , editorParser =
      \input ->
        parseTextWith operatorParser "operator" (" " <> T.pack input <> " ")
  , editorRenderer =
      \(string, op) -> do
        text string
        pure (updated (constDyn (Just (Right (string, op)))))
  }

--------------------------------------------------------------------------------
-- Parameter editor

parametersEditor
  :: MonadWidget t m
  => Editor t m [Pattern UnkindedType Identifier Location]
parametersEditor =
  Editor
  { editorPrinter = unwords . map (printPat defaultPrint)
  , editorParser = parseTextWith funcParams "parameters" . T.pack
  , editorRenderer =
      \params -> do
        text (unwords (map (printPat defaultPrint) params))
        pure (updated (constDyn (Just (Right params))))
  }

--------------------------------------------------------------------------------
-- Pattern editor

patternEditor
  :: MonadWidget t m
  => Editor t m (Pattern UnkindedType Identifier Location)
patternEditor =
  Editor
  { editorPrinter = printPat defaultPrint
  , editorParser = parseTextWith altPat "pattern" . T.pack
  , editorRenderer =
      \pat -> do
        text (printPat defaultPrint pat)
        pure (updated (constDyn (Just (Right pat))))
  }

--------------------------------------------------------------------------------
-- Alt editor

alternativeEditor
  :: MonadWidget t m
  => Editor t m (Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)
alternativeEditor =
  Editor
  { editorPrinter = printAlt defaultPrint
  , editorParser = parseTextWith (altParser Nothing 0) "alternative" . T.pack
  , editorRenderer =
      \(pat, expr) -> do
        patDyn <- runEditor patternEditor (Just pat) >>= holdDyn pat
        text " -> "
        exprDyn <- runEditor expressionEditor (Just expr) >>= holdDyn expr
        altDyn <- combineDyn (,) patDyn exprDyn
        bubble altDyn
  }

alternativesEditor
  :: MonadWidget t m
  => Editor t m [(Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)]
alternativesEditor =
  Editor
  { editorPrinter = unlines . map (printAlt defaultPrint)
  , editorParser = parseTextWith altsParser "alternatives" . T.pack
  , editorRenderer =
      \alts -> do
        altDyns <-
          mapM
            (\alt ->
               el "div" (runEditor alternativeEditor (Just alt)) >>=
               holdDyn [alt] . fmapMaybe (pure . pure))
            alts
        altsDyn <- mconcatDyn altDyns
        bubble altsDyn
  }

--------------------------------------------------------------------------------
-- Editor combinators

data Editor t m a = Editor
  { editorPrinter :: a -> String
  , editorParser :: String -> Either SomeException a
  , editorRenderer :: a -> m (Event t (Maybe (Either SomeException a)))
  }

-- | Produce an editor from a printer, parser and renderer.
runEditor
  :: MonadWidget t m
  => Editor t m a
  -> Maybe a
  -> m (Event t a)
runEditor (Editor printer parser renderer) mdef = do
  inputWidget <-
    textInput def {_textInputConfig_initialValue = maybe "" printer mdef}
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
         Just (Right e) -> renderer e)
      parseResultDyn
  streamsEv <- dyn widgetDyn
  currentValuesEv <- switchPromptly never streamsEv
  pure
    (fmapMaybe
       (>>= either (const Nothing) Just)
       (leftmost [updated parseResultDyn, currentValuesEv]))

-- | Bubble the value of an AST element upwards as a correctly parsed,
-- available value.
bubble
  :: MonadWidget t m
  => Dynamic t a -> m (Event t (Maybe (Either SomeException a)))
bubble = pure . fmapMaybe (Just . Just . Right) . updated
