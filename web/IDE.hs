{-# LANGUAGE ScopedTypeVariables #-}
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
import           Reflex.Class hiding (constant)
import           Reflex.Dom (MonadWidget)
import qualified Reflex.Dom as Dom
import           Reflex.Dynamic

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main =
  Dom.mainWidget
    (do rec let focusDyn = constDyn oid
            (astEl, (astOut, oid)) <-
              Dom.el'
                "div"
                (newEditor
                   expressionEditor
                   Input {inputFocus = focusDyn, inputValue = never}
                   wildcard
                   ExpressionNode)
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
        astDyn <- holdDyn wildcard (outputValue astOut)
        printedDyn <- mapDyn (printExpression defaultPrint) astDyn
        metaDyn <- mapDyn show (outputMeta astOut)
        Dom.el "div" (Dom.dynText metaDyn)
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
      constant e
    LiteralExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      constant e
    ConstructorExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      constant e
    ConstantExpression {} -> do
      Dom.text (printExpression defaultPrint e)
      constant e
    ApplicationExpression l f x -> do
      Dom.text "("
      (fDyn, fid) <- child f
      (xDyn, xid) <- child x
      Dom.text ")"
      appsDyn <- combineOutput (ApplicationExpression l) fDyn f xDyn x
      pure appsDyn
    IfExpression l cond then' else' -> do
      Dom.text "if"
      (condDyn, cid) <- child cond
      Dom.text "then"
      (thenDyn, tid) <- child then'
      Dom.text "else"
      (elseDyn, eid) <- child else'
      makeIfDyn <- combineOutput (IfExpression l) condDyn cond thenDyn then'
      ifDyn <-
        combineOutput ($) makeIfDyn (IfExpression l cond then') elseDyn else'
      pure ifDyn
    InfixExpression l left op right -> do
      (leftDyn, lid) <- child left
      (opDyn, xid) <- newEditor operatorEditor input op OperatorNode
      (rightDyn, rid) <- child right
      makeOpDyn <- combineOutput (InfixExpression l) leftDyn left opDyn op
      opDyn <-
        combineOutput ($) makeOpDyn (InfixExpression l left op) rightDyn right
      pure opDyn
    LambdaExpression l (Alternative l' params expr) -> do
      Dom.text "\\"
      (paramsDyn, xid) <- newEditor parametersEditor input params ParametersNode
      Dom.text "->"
      (exprDyn, eid) <- child expr
      lamDyn <-
        combineOutput
          (\ps e -> LambdaExpression l (Alternative l' ps e))
          paramsDyn
          params
          exprDyn
          expr
      pure lamDyn
    CaseExpression l expr alts -> do
      Dom.text "case"
      (exprDyn, eid) <- child expr
      Dom.text "of"
      (altsDyn, xid) <- newEditor alternativesEditor input alts AlternativesNode
      caseDyn <- combineOutput (CaseExpression l) exprDyn expr altsDyn alts
      pure caseDyn
    _ -> do
      Dom.divClass "warning" (Dom.text ("Unsupported node type: " <> show e))
      pure
        Output
        {outputValue = updated (constDyn e), outputMeta = constDyn mempty}
  where
    child v = newEditor expressionEditor input v ExpressionNode

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
        constant (string, op)
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
        constant param
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
               (output, id) <- newEditor parameterEditor input param ParameterNode
               Dom.text " "
               valueDyn <-
                 holdDyn [param] (fmapMaybe (pure . pure) (outputValue output))
               pure (id, valueDyn, outputMeta output))
            params
        value <- mconcatDyn (map (\(_,v,_) -> v) paramDyns)
        meta <- mconcatDyn (map (\(_,_,g) -> g) paramDyns)
        pure
          Output
          {outputValue = updated value, outputMeta = meta}
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
        constant pat
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
        (patDyn, xid) <- newEditor patternEditor input pat PatternNode
        Dom.text " -> "
        (exprDyn, xid) <- newEditor expressionEditor input expr ExpressionNode
        altDyn <- combineOutput (,) patDyn pat exprDyn expr
        pure altDyn
  }

alternativesEditor
  :: MonadWidget t m
  => Editor t m [(Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)]
alternativesEditor =
  Editor
  { editorPrinter = unlines . map (printAlt defaultPrint)
  , editorParser =
      parseTextWith altsParser "alternatives" .
      T.unlines . map (" " <>) . T.lines . T.pack
  , editorRenderer =
      \input uuid alts -> do
        altDyns <-
          mapM
            (\alt -> do
               (output, id) <-
                 newEditor alternativeEditor input alt AlternativeNode
               Dom.text " "
               valueDyn <-
                 holdDyn [alt] (fmapMaybe (pure . pure) (outputValue output))
               pure (id, valueDyn, outputMeta output))
            alts
        value <- mconcatDyn (map (\(_, v, _) -> v) altDyns)
        meta <- mconcatDyn (map (\(_, _, g) -> g) altDyns)
        pure Output {outputValue = updated value, outputMeta = meta}
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
  { inputValue :: Event t (ID, Node)
  , inputFocus :: Dynamic t ID
  }

-- | A value that can be set to any editor.
data Node
  = ExpressionNode (Expression UnkindedType Identifier Location)
  | AlternativeNode (Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)
  | AlternativesNode [(Pattern UnkindedType Identifier Location, Expression UnkindedType Identifier Location)]
  | PatternNode (Pattern UnkindedType Identifier Location)
  | OperatorNode (String, Expression UnkindedType Identifier Location)
  | ParametersNode [Pattern UnkindedType Identifier Location]
  | ParameterNode (Pattern UnkindedType Identifier Location)
  deriving (Show)

-- | An editor's output.
data Output t a = Output
  { outputValue :: Event t a
  , outputMeta :: Dynamic t (Map ID Meta)
  }

-- | Metadata about an editor.
data Meta = Meta
  { metaParent :: Maybe ID
  , metaChildren :: [ID]
  , metaNode :: !Node
  } deriving (Show)

-- | A globally unique ID for an editor.
newtype ID = ID String deriving (Eq, Ord, Show)

-- | Produce an editor from a printer, parser and renderer.
newEditor
  :: forall t m a.
     MonadWidget t m
  => Editor t m a -> Input t -> a -> (a -> Node) -> m (Output t a, ID)
newEditor (Editor printer parser renderer) input@(Input value focus) initialValue cons = do
  uuid <- liftIO (fmap ID generateUUID)
  rec inputAttrs <- visibilityToggler (== uuid) focus
      inputWidget <-
        Dom.divClass
          "duet-input"
          (Dom.textArea
             (def :: Dom.TextAreaConfig t)
             { Dom._textAreaConfig_initialValue = printer initialValue
             , Dom._textAreaConfig_attributes = inputAttrs
             , Dom._textAreaConfig_setValue =
                 fmapMaybe (pure . printer) currentValuesEv
             })
      parseResultDyn <-
        foldDyn
          (const . parser)
          (Right initialValue)
          (Dom._textArea_input inputWidget)
      errMsgAttrs <- visibilityToggler (== uuid) focus
      widgetAttrs <- visibilityToggler (/= uuid) focus
      widgetDyn <-
        mapDyn
          (\case
             (Left e) -> do
               Dom.elDynAttr
                 "div"
                 errMsgAttrs
                 (Dom.divClass
                    "bg-danger"
                    (Dom.text (show (e :: SomeException))))
               pure Output {outputValue = never, outputMeta = constDyn mempty}
             (Right e) ->
               Dom.elDynAttr "div" widgetAttrs (renderer input uuid e))
          parseResultDyn
      streamsEv <- Dom.dyn widgetDyn
      currentValuesEv <-
        switchPromptly never (fmapMaybe (pure . outputValue) streamsEv)
  metas <- holdDyn (constDyn mempty) (fmapMaybe (pure . outputMeta) streamsEv)
  metaDyn <-
    mapDyn
      (M.insert
         uuid
         (Meta
          { metaNode = cons initialValue
          , metaParent = Nothing
          , metaChildren = mempty
          }))
      (joinDyn metas)
  pure
    ( Output
      { outputValue =
          fmapMaybe
            (either (const Nothing) Just)
            (leftmost
               [ updated parseResultDyn
               , fmapMaybe (pure . Right) currentValuesEv
               ])
      , outputMeta = metaDyn
      }
    , uuid)
  where
    visibilityToggler f =
      mapDyn
        (\i ->
           M.fromList
             [ ( "style" :: String
               , if f i
                   then ""
                   else "display:none" :: String)
             ])

-- | The wildcard or "empty slot" expression.
wildcard :: Expression UnkindedType Identifier Location
wildcard = ConstantExpression (Location 0 0 0 0) "_"

-- | Combine two editor outputs to form one.
combineOutput
  :: MonadWidget t m
  => (a -> b -> c) -> Output t a -> a -> Output t b -> b -> m (Output t c)
combineOutput f a adef b bdef = do
  aDyn <- holdDyn adef (outputValue a)
  bDyn <- holdDyn bdef (outputValue b)
  cDyn <- combineDyn f aDyn bDyn
  metaDyn <- combineDyn M.union (outputMeta a) (outputMeta b)
  pure Output {outputValue = updated cDyn, outputMeta = metaDyn}

-- | Produce a constant output.
constant :: MonadWidget t m => e -> m (Output t e)
constant e = do
  pure
    Output
    { outputValue = updated (constDyn e)
    , outputMeta = constDyn mempty
    }
