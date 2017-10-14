{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID
import           Duet.Parser hiding (atomic)
import           Duet.Printer
import           Duet.Types
import           Reflex.Class
import           Reflex.Dom (MonadWidget)
import qualified Reflex.Dom as Dom
import           Reflex.Dynamic

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  Dom.mainWidget
    (do rec let focusDyn = constDyn (outputID astOut)
            (astEl, astOut) <-
              Dom.el'
                "div"
                (newEditor
                   expressionEditor
                   Input {inputFocus = focusDyn, inputValue = never}
                   wildcard)
        keysDyn <-
          holdDyn
            "No keys down."
            (fmapMaybe (pure . show) (Dom.domEvent Dom.Keydown astEl))
        Dom.el "div" (Dom.dynText keysDyn)
        keysDyn <-
          holdDyn
            "No keys pressed."
            (fmapMaybe (pure . show) (Dom.domEvent Dom.Keypress astEl))
        Dom.el "div" (Dom.dynText keysDyn)
        astDyn <- holdDyn Nothing (fmapMaybe (pure . pure) (outputValue astOut))
        printedDyn <-
          mapDyn
            (maybe "No AST currently." (printExpression defaultPrint))
            astDyn
        Dom.el "div" (Dom.dynText printedDyn))

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
  => Input t
  -> ID
  -> Expression UnkindedType Identifier Location
  -> m (Output t (Expression UnkindedType Identifier Location))
renderExpression input uuid e =
  case e of
    VariableExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      atomic e
    LiteralExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      atomic e
    ConstructorExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      atomic e
    ConstantExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      atomic e
    ApplicationExpression l f x -> do
      Dom.text "("
      fDyn <- child f
      xDyn <- child x
      Dom.text ")"
      appsDyn <- combineOutput (ApplicationExpression l) fDyn xDyn
      pure appsDyn
    IfExpression l cond then' else' -> do
      Dom.text "if"
      condDyn <- child cond
      Dom.text "then"
      thenDyn <- child then'
      Dom.text "else"
      elseDyn <- child else'
      makeIfDyn <- combineOutput (IfExpression l) condDyn thenDyn
      ifDyn <- combineOutput ($) makeIfDyn elseDyn
      pure ifDyn
    InfixExpression l left op right -> do
      leftDyn <- child left
      opDyn <- newEditor operatorEditor input op
      rightDyn <- child right
      makeOpDyn <- combineOutput (InfixExpression l) leftDyn opDyn
      opDyn <- combineOutput ($) makeOpDyn rightDyn
      pure opDyn
    LambdaExpression l (Alternative l' params expr) -> do
      Dom.text "\\"
      paramsDyn <- newEditor parametersEditor input params
      Dom.text "->"
      exprDyn <- child expr
      lamDyn <-
        combineOutput
          (\ps e -> LambdaExpression l (Alternative l' ps e))
          paramsDyn
          exprDyn
      pure lamDyn
    CaseExpression l expr alts -> do
      Dom.text "case"
      exprDyn <- child expr
      Dom.text "of"
      altsDyn <- newEditor alternativesEditor input alts
      caseDyn <- combineOutput (CaseExpression l) exprDyn altsDyn
      pure caseDyn
    _ -> do
      Dom.divClass "warning" (Dom.text ("Unsupported node type: " <> show e))
      pure
        Output
        { outputConst = e
        , outputValue = updated (constDyn e)
        , outputGenealogy = constDyn mempty
        }
  where
    child v = newEditor expressionEditor input v

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
      \_ _ (string, op) -> do
        Dom.text string
        atomic (string, op)
  }

--------------------------------------------------------------------------------
-- Parameter editor

parameterEditor
  :: MonadWidget t m
  => Editor t m (Pattern UnkindedType Identifier Location)
parameterEditor =
  Editor
  { editorPrinter = printPat defaultPrint
  , editorParser = parseTextWith funcParam "parameter" . T.pack
  , editorRenderer =
      \_ _ param -> do
        Dom.text (printPat defaultPrint param)
        atomic param
  }

parametersEditor
  :: MonadWidget t m
  => Editor t m [Pattern UnkindedType Identifier Location]
parametersEditor =
  Editor
  { editorPrinter = unwords . map (printPat defaultPrint)
  , editorParser = parseTextWith funcParams "parameters" . T.pack
  , editorRenderer =
      \input uuid params -> do
        paramDyns <-
          mapM
            (\param -> do
               e <- newEditor parameterEditor input param
               Dom.text " "
              -- holdDyn [param] (fmapMaybe (pure . pure) e)
               undefined)
            params
        undefined paramDyns
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
      \_ _ pat -> do
        Dom.text (printPat defaultPrint pat)
        atomic pat
  }

--------------------------------------------------------------------------------
-- Alt editor

alternativeEditor
  :: MonadWidget t m
  => Editor t m (Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)
alternativeEditor =
  Editor
  { editorPrinter = printAlt defaultPrint
  , editorParser = parseTextWith (altParser Nothing 1) "alternative" . T.pack
  , editorRenderer =
      \input uuid (pat, expr) -> do
        patDyn <- newEditor patternEditor input pat
        Dom.text " -> "
        exprDyn <- newEditor expressionEditor input expr
        altDyn <- combineOutput (,) patDyn exprDyn
        pure altDyn
  }

alternativesEditor
  :: MonadWidget t m
  => Editor t m [(Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)]
alternativesEditor =
  Editor
  { editorPrinter = unlines . map (printAlt defaultPrint)
  , editorParser = parseTextWith altsParser "alternatives" . T.pack
  , editorRenderer =
      \input _ alts -> do
        altDyns <-
          mapM
            (\alt -> Dom.el "div" (newEditor alternativeEditor input alt))
            alts
        undefined altDyns
  }

--------------------------------------------------------------------------------
-- Editor combinators

-- | An editor's definition: print, parse, render.
data Editor t m a = Editor
  { editorPrinter :: a -> String
  , editorParser :: String -> Either SomeException a
  , editorRenderer :: Input t -> ID -> a -> m (Output t a)
  }

-- | Input to all editors.
data Input t = Input
  { inputValue :: Event t (ID, Value)
  , inputFocus :: Dynamic t ID
  }

-- | A value that can be set to any editor.
data Value

-- | An editor's output.
data Output t a = Output
  { outputValue :: Event t a
  , outputGenealogy :: Dynamic t (Map ID Genealogy)
  , outputConst :: a
  , outputID :: ID
  }

-- | An editor's ancestry and lineage.
data Genealogy = Genealogy
  { genealogyParent :: Maybe ID
  , genealogyChildren :: [ID]
  }

-- | A globally unique ID for an editor.
newtype ID = ID String deriving (Eq, Ord)

-- | Produce an editor from a printer, parser and renderer.
newEditor
  :: MonadWidget t m
  => Editor t m a -> Input t -> a -> m (Output t a)
newEditor (Editor printer parser renderer) input@(Input value focus) initialVal = do
  uuid <- liftIO (fmap ID generateUUID)
  rec inputWidget <-
        Dom.divClass
          "duet-input"
          (Dom.textArea
             def
             { Dom._textAreaConfig_setValue =
                 fmapMaybe
                   (either (const Nothing) (Just . printer))
                   currentValuesEv
             })
      parseResultDyn <-
        foldDyn
          (const . parser)
          (Right initialVal)
          (Dom._textArea_input inputWidget)
      widgetDyn <-
        mapDyn
          (\case
             (Left e) -> do
               Dom.divClass "bg-danger" (Dom.text (show (e :: SomeException)))
               atomic initialVal
             (Right e) -> renderer input uuid e)
          (undefined parseResultDyn)
      streamsEv <- Dom.dyn widgetDyn
      currentValuesEv <- switchPromptly never (undefined streamsEv)
  pure
    (Output
     { outputValue =
         fmapMaybe
           (either (const Nothing) Just)
           (leftmost [updated parseResultDyn, undefined currentValuesEv])
     , outputGenealogy = constDyn mempty
     , outputID = uuid
     })

-- | The wildcard or "empty slot" expression.
wildcard :: Expression UnkindedType Identifier Location
wildcard = ConstantExpression (Location 0 0 0 0) "_"

-- | Combine two editor outputs to form one.
combineOutput
  :: MonadWidget t m
  => (a -> b -> c) -> Output t a -> Output t b -> m (Output t c)
combineOutput f a b = do
  aDyn <- holdDyn (outputConst a) (outputValue a)
  bDyn <- holdDyn (outputConst b) (outputValue b)
  cDyn <- combineDyn f aDyn bDyn
  pure
    Output
    { outputConst = f (outputConst a) (outputConst b)
    , outputValue = updated cDyn
    , outputGenealogy = undefined -- M.union (outputGenealogy a) (outputGenealogy b)
    }

atomic e = do
  pure
    Output
    { outputConst = e
    , outputValue = updated (constDyn e)
    , outputGenealogy = constDyn mempty
    , outputID = undefined
    }
