{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

-- |

module Duet.IDE.View where

import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.IDE.Types
import           Duet.Printer (printImplicitlyTypedBinding, defaultPrint)
import           Duet.Types
import           React.Flux ((@=))
import           React.Flux (ViewEventHandler)
import qualified React.Flux as Flux
import           React.Flux.Internal (ReactElementM)

renderModule :: State -> Node -> ReactElementM ViewEventHandler ()
renderModule state node = do
  renderNode state node
  when
    debug
    (Flux.p_
       ["key" @= "pretty-printed"]
       (Flux.text_
          (Flux.elemText
             (T.pack
                (case node of
                   DeclNode (BindDecl _ (ImplicitBinding i)) ->
                     printImplicitlyTypedBinding defaultPrint i
                   _ -> "Nothing available to print.")))))
  when
    debug
    (Flux.p_
       ["key" @= "shown"]
       (Flux.text_ (Flux.elemText (T.pack (show node)))))
  where
    debug = False

renderNode :: State -> Node -> ReactElementM ViewEventHandler ()
renderNode state =
  \case
    ExpressionNode n -> renderExpression state n
    DeclNode d -> renderDecl state d
    ModuleNode _ ds ->
      mapM_
        (\d -> renderDecl state d)
        ds
    NameNode d -> renderBinding state d
    OperatorNode l d -> renderOperator state l d
    PatternNode p -> renderPattern state p
    AltNode _ -> pure ()

renderOperator :: forall eventHandler handler t. Flux.Term eventHandler [Flux.PropertyOrHandler handler] (ReactElementM ViewEventHandler () -> t) => State -> Label -> Identifier -> t
renderOperator state l op =
  Flux.span_
    ["className" @= "duet-op", "key" @= "op"]
    (renderExpression state (VariableExpression l op))

renderDecl :: State -> Decl UnkindedType Identifier Label -> ReactElementM ViewEventHandler ()
renderDecl state =
  \case
    BindDecl label (ImplicitBinding implicit) ->
      renderWrap
        state
        label
        "duet-declaration"
        (renderImplicitBinding state implicit)
    _ -> pure ()

renderImplicitBinding :: State -> ImplicitlyTypedBinding UnkindedType Identifier Label -> ReactElementM ViewEventHandler ()
renderImplicitBinding state (ImplicitlyTypedBinding label binding a) =
  renderWrap
    state
    label
    "duet-binding duet-implicit-binding"
    (mapM_ (renderAlternative state True (Just binding)) a)

renderBinding :: State -> (Identifier, Label) -> ReactElementM ViewEventHandler ()
renderBinding state (Identifier i, label') =
  renderWrap
    state
    label'
    ("duet-binding-name" <>
     if i == "_"
       then " duet-pattern-wildcard"
       else "")
    (Flux.elemText (T.pack i))

renderAlternative :: State -> Bool -> Maybe (Identifier, Label) -> Duet.Types.Alternative UnkindedType Identifier Label -> ReactElementM ViewEventHandler ()
renderAlternative state equals mbinding (Alternative label pats e) =
  renderWrap
    state
    label
    "duet-alternative"
    (do maybe (return ()) (renderBinding state) mbinding
        mapM_ (renderPattern state) pats
        if not equals
          then Flux.span_
                 ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
                 (Flux.elemText "→")
          else Flux.span_
                 ["className" @= "duet-keyword", "key" @= "equals"]
                 (Flux.elemText "=")
        Flux.br_ ["key" @= "alt-break"]
        Flux.span_
          ["className" @= "duet-rhs", "key" @= "alt-expr"]
          (renderExpression state e))

renderExpression
  :: State
  -> Expression UnkindedType Identifier Label
  -> ReactElementM ViewEventHandler ()
renderExpression state =
  \case
    VariableExpression label (Identifier ident) ->
      renderExpr label "duet-variable" (Flux.elemText (T.pack ident))
    LiteralExpression label lit -> renderLiteral state label lit
    ParensExpression label e ->
      renderExpr label "duet-parens" (renderExpression state e)
    app@(ApplicationExpression label _ _) ->
      renderExpr
        label
        "duet-application"
        (do let (f, xs) = fargs app
            case f of
              ApplicationExpression {} -> renderExpression state f
              _ -> parens "func" f (renderExpression state f)
            if any lineBreaks xs
              then indented
                     "app"
                     (mapM_
                        (\(i, x) -> do
                           unless
                             (i == 1)
                             (Flux.br_ ["key" @= ("app-break-" ++ show i)])
                           parens
                             ("app-" ++ show i)
                             x
                             (renderExpression state x))
                        (zip [1 ..] xs))
              else mapM_
                     (\(i, x) ->
                        parens ("app-" ++ show i) x (renderExpression state x))
                     (zip [1 ..] xs))
    InfixExpression label f (_, VariableExpression l op) x ->
      renderExpr
        label
        "duet-infix"
        (do renderExpression state f
            renderOperator state l op
            renderExpression state x)
    ConstantExpression label (Identifier ident) ->
      renderExpr label "duet-constant" (Flux.elemText (T.pack ident))
    IfExpression label e f g ->
      renderExpr
        label
        "duet-if"
        (do Flux.span_
              ["className" @= "duet-keyword", "key" @= "if"]
              (Flux.elemText "if")
            renderExpressionIndented "if" state e
            Flux.br_ ["key" @= "then-break"]
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "then"]
              (Flux.elemText "then")
            renderExpressionIndented "then" state f
            Flux.br_ ["key" @= "else-break"]
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "else"]
              (Flux.elemText "else")
            renderExpressionIndented "else" state g)
    CaseExpression label e alts ->
      renderExpr
        label
        "duet-case"
        (do Flux.span_
              ["className" @= "duet-keyword", "key" @= "case"]
              (Flux.elemText "case")
            if lineBreaks e
              then do
                renderExpressionIndented "case-expr" state e
                Flux.br_ ["key" @= "else-break"]
              else renderExpression state e
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "of"]
              (Flux.elemText "of")
            Flux.br_ ["key" @= "of-rhs-break"]
            Flux.span_
              ["className" @= "duet-rhs", "key" @= "rhs"]
              (mapM_
                 (\(i, (CaseAlt l pat expr)) -> do
                    unless
                      (i == 1)
                      (Flux.br_ ["key" @= ("pat-break-" ++ show i)])
                    renderExpr
                      l
                      "duet-case-alt"
                      (do renderPattern state pat
                          Flux.span_
                            [ "className" @= "duet-keyword duet-arrow"
                            , "key" @= ("arrow" ++ show i)
                            ]
                            (Flux.elemText "→")
                          Flux.br_ ["key" @= ("arrow-break-" ++ show i)]
                          Flux.span_
                            [ "className" @= "duet-rhs"
                            , "key" @= ("rhs-" ++ show i)
                            ]
                            (renderExpression state expr)))
                 (zip [1 ..] alts)))
    LambdaExpression label (Alternative _ ps e) ->
      renderExpr
        label
        "duet-lambda"
        (do Flux.span_
              ["className" @= "duet-lambda duet-keyword", "key" @= "backslash"]
              (Flux.elemText "\\")
            mapM_ (renderPattern state) ps
            Flux.span_
              ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
              (Flux.elemText "→")
            Flux.br_ ["key" @= "lambda-br"]
            Flux.span_
              ["className" @= "duet-rhs", "key" @= "lambda-rhs"]
              (renderExpression state e))
    _ -> pure ()
  where
    renderExpr label className' =
      renderWrap state label ("duet-expression " <> className')

-- | Flatten an application f x y into (f,[x,y]).
fargs :: Expression t i l -> (Expression t i l, [(Expression t i l)])
fargs e = go e []
  where
    go (ApplicationExpression _ f x) args = go f (x : args)
    go f args = (f, args)

renderExpressionIndented :: [Char] -> State -> Expression UnkindedType Identifier Label -> ReactElementM ViewEventHandler ()
renderExpressionIndented prefix state e =
  if lineBreaks e
    then indented prefix (renderExpression state e)
    else renderExpression state e

indented :: forall eventHandler handler t b handler1. Flux.Term eventHandler [Flux.PropertyOrHandler handler] (ReactElementM handler1 b -> t) => [Char] -> ReactElementM handler1 b -> t
indented prefix m = do
  Flux.span_
    ["key" @= (prefix ++ "-indented-wrap")]
    (do Flux.br_ ["key" @= (prefix ++ "-indented-break")]
        Flux.div_
          ["key" @= (prefix ++ "-indented-padding"), "className" @= "duet-indented"]
          m)

renderLiteral :: State -> Label -> Literal -> ReactElementM ViewEventHandler ()
renderLiteral state label lit =
  case lit of
    IntegerLiteral i ->
      renderExpr  "duet-integer" (Flux.elemText (T.pack (show i)))
    _ -> pure ()
  where renderExpr  className' =
              renderWrap state label ("duet-expression " <> className')

lineBreaks :: Expression x y z -> Bool
lineBreaks =
  \case
    ApplicationExpression _ x y -> lineBreaks x || lineBreaks y
    InfixExpression _ x _ y -> lineBreaks x || lineBreaks y
    LambdaExpression {} -> True
    IfExpression {} -> True
    CaseExpression {} -> True
    ParensExpression _ e -> lineBreaks e
    _ -> False

parens
  :: String
  -> Expression UnkindedType Identifier Label
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
parens prefix e m =
  if atomic e
    then m
    else do
      Flux.span_
        [ "key" @= (prefix ++ "-parens")
        , "data-key" @= (prefix ++ "-parens")
        , "className" @= ("duet-node duet-implicit-parens")
        ]
        m

atomic :: Expression t i l -> Bool
atomic e =
  case e of
    VariableExpression {} -> True
    ConstantExpression {} -> True
    CaseExpression {} -> False
    ApplicationExpression {} -> False
    IfExpression {} -> False
    LambdaExpression {} -> False
    LetExpression {} -> False
    InfixExpression {} -> False
    LiteralExpression {} -> True
    ConstructorExpression {} -> True
    ParensExpression {} -> True

renderPattern
  :: State
  -> Pattern UnkindedType Identifier Label
  -> ReactElementM ViewEventHandler ()
renderPattern state =
  \case
    WildcardPattern label string ->
      renderWrap
        state
        label
        "duet-pattern duet-pattern-wildcard"
        (Flux.elemText (T.pack string))
    VariablePattern label (Identifier string) ->
      renderWrap
        state
        label
        "duet-pattern duet-pattern-variable"
        (Flux.elemText (T.pack string))
    LiteralPattern label lit ->
      renderWrap
        state
        label
        "duet-pattern duet-pattern-literal"
        (renderLiteral state label lit)
    _ -> pure ()

renderWrap
  :: State
  -> Label
  -> Text
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
renderWrap state label className' =
  Flux.span_
    [ "key" @= labelUUID label
    , "data-key" @= labelUUID label
    , "className" @=
      ("duet-node " <> className' <> " " <>
       (if (labelUUID label) == cursorUUID (stateCursor state)
          then "duet-selected"
          else "duet-unselected") <> " " <>
       if M.member (labelUUID label) (stateHighlightErrors state)
         then "duet-erroneous"
         else "")
    ]
