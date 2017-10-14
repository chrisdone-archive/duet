{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Parser
import           Duet.Printer
import           Duet.Types
import           Reflex.Dom
import           Reflex.Dom.Widget.Advanced

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  mainWidget
    (do (astEl, astEv) <- el' "div" (newEditorEvent expressionEditor Nothing)
        astDyn <- holdDyn Nothing (fmap Just astEv)
        keysDyn <-
          holdDyn
            "No keys down."
            (fmapMaybe (pure . show) (domEvent Keydown astEl))
        el "div" (dynText keysDyn)
        keysDyn <-
          holdDyn
            "No keys pressed."
            (fmapMaybe (pure . show) (domEvent Keypress astEl))
        el "div" (dynText keysDyn)
        printedDyn <-
          mapDyn
            (maybe "No AST currently." (printExpression defaultPrint))
            astDyn
        el "div" (dynText printedDyn))

--------------------------------------------------------------------------------
-- Expression editing

expressionEditor
  :: MonadWidget t m
  => Editor t m f (Expression UnkindedType Identifier Location)
expressionEditor =
  Editor
  { editorPrinter = printExpression defaultPrint
  , editorParser = parseTextWith expParser "expression" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer = renderExpression
  }

renderExpression
  :: MonadWidget t m
  => f
  -> Expression UnkindedType Identifier Location
  -> m (Event t (Maybe (Either SomeException (Expression UnkindedType Identifier Location))))
renderExpression _ e =
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
      opDyn <- newEditorDynamic operatorEditor op
      rightDyn <- child right
      makeOpDyn <- combineDyn (InfixExpression l) leftDyn opDyn
      opDyn <- combineDyn ($) makeOpDyn rightDyn
      bubble opDyn
    LambdaExpression l (Alternative l' params expr) -> do
      text "\\"
      paramsDyn <- newEditorDynamic parametersEditor params
      text "->"
      exprDyn <- child expr
      lamDyn <-
        combineDyn
          (\ps e -> LambdaExpression l (Alternative l' ps e))
          paramsDyn
          exprDyn
      bubble lamDyn
    CaseExpression l expr alts ->
      do text "case"
         exprDyn <- child expr
         text "of"
         altsDyn <- newEditorDynamic alternativesEditor alts
         caseDyn <- combineDyn (CaseExpression l) exprDyn altsDyn
         bubble caseDyn
    _ -> do
      divClass "warning" (text ("Unsupported node type: " <> show e))
      pure (updated (constDyn (Just (Right e))))
  where
    atomic = do
      text (printExpression defaultPrint e)
      pure (updated (constDyn (Just (Right e))))
    child v = newEditorDynamic expressionEditor v

--------------------------------------------------------------------------------
-- Operator editing

operatorEditor
  :: MonadWidget t m
  => Editor t m f (String, Expression UnkindedType Identifier Location)
operatorEditor =
  Editor
  { editorPrinter = \(string, _op) -> string
  , editorParser =
      \input ->
        parseTextWith operatorParser "operator" (" " <> T.pack input <> " ")
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ (string, op) -> do
        text string
        pure (updated (constDyn (Just (Right (string, op)))))
  }

--------------------------------------------------------------------------------
-- Parameter editor

parameterEditor
  :: MonadWidget t m
  => Editor t m f (Pattern UnkindedType Identifier Location)
parameterEditor =
  Editor
  { editorPrinter = printPat defaultPrint
  , editorParser = parseTextWith funcParam "parameter" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ param -> do
        text (printPat defaultPrint param)
        pure (updated (constDyn (Just (Right param))))
  }

parametersEditor
  :: MonadWidget t m
  => Editor t m f [Pattern UnkindedType Identifier Location]
parametersEditor =
  Editor
  { editorPrinter = unwords . map (printPat defaultPrint)
  , editorParser = parseTextWith funcParams "parameters" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ params -> do
        paramDyns <-
          mapM
            (\param -> do
               e <- newEditorEvent parameterEditor (Just param)
               text " "
               holdDyn [param] (fmapMaybe (pure . pure) e))
            params
        paramsDyn <- mconcatDyn paramDyns
        bubble paramsDyn
  }

--------------------------------------------------------------------------------
-- Pattern editor

patternEditor
  :: MonadWidget t m
  => Editor t m f (Pattern UnkindedType Identifier Location)
patternEditor =
  Editor
  { editorPrinter = printPat defaultPrint
  , editorParser = parseTextWith altPat "pattern" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ pat -> do
        text (printPat defaultPrint pat)
        pure (updated (constDyn (Just (Right pat))))
  }

--------------------------------------------------------------------------------
-- Alt editor

alternativeEditor
  :: MonadWidget t m
  => Editor t m f (Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)
alternativeEditor =
  Editor
  { editorPrinter = printAlt defaultPrint
  , editorParser = parseTextWith (altParser Nothing 1) "alternative" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ (pat, expr) -> do
        patDyn <- newEditorDynamic patternEditor pat
        text " -> "
        exprDyn <- newEditorDynamic expressionEditor expr
        altDyn <- combineDyn (,) patDyn exprDyn
        bubble altDyn
  }

alternativesEditor
  :: MonadWidget t m
  => Editor t m f [(Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)]
alternativesEditor =
  Editor
  { editorPrinter = unlines . map (printAlt defaultPrint)
  , editorParser = parseTextWith altsParser "alternatives" . T.pack
  , editorHandler = \_key _input _a -> Nothing
  , editorRenderer =
      \_ alts -> do
        altDyns <-
          mapM
            (\alt ->
               el "div" (newEditorEvent alternativeEditor (Just alt)) >>=
               holdDyn [alt] . fmapMaybe (pure . pure))
            alts
        altsDyn <- mconcatDyn altDyns
        bubble altsDyn
  }

--------------------------------------------------------------------------------
-- Editor combinators

-- | An editor's definition: print, parse, render.
data Editor t m f a = Editor
  { editorPrinter :: a -> String
  , editorParser :: String -> Either SomeException a
  , editorRenderer :: Maybe f -> a -> m (Event t (Maybe (Either SomeException a)))
  , editorHandler :: Int -> String -> a -> Maybe (f, a)
  }

-- | Run an editor with a definite value
newEditorDynamic
  :: MonadWidget t m
  => Editor t m f a -> a -> m (Dynamic t a)
newEditorDynamic e a = newEditorEvent e (Just a) >>= holdDyn a

-- | Produce an editor from a printer, parser and renderer.
newEditorEvent
  :: MonadWidget t m
  => Editor t m f a -> Maybe a -> m (Event t a)
newEditorEvent (Editor printer parser renderer handler) mdef = do
  rec inputWidget <-
        divClass
          "duet-input"
          (textArea
             def
             { _textAreaConfig_initialValue = maybe "" printer mdef
             , _textAreaConfig_setValue =
                 fmapMaybe
                   (>>= either (const Nothing) (Just . printer))
                   currentValuesEv
             })
      parseResultDyn <-
        foldDyn
          (const . Just . parser)
          (fmap Right mdef)
          (_textArea_input inputWidget)
      widgetDyn <-
        mapDyn
          (\case
             Nothing -> pure (updated (constDyn Nothing))
             Just (Left e) -> do
               divClass "bg-danger" (text (show e))
               pure (updated (constDyn (Just (Left e))))
             Just (Right e) -> renderer Nothing e)
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
