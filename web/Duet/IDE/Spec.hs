-- |

module Duet.IDE.Spec where

import Data.Monoid
import Duet.IDE
import Duet.IDE.Test
import Duet.IDE.Types
import Duet.Types
import React.Flux.Persist

tests :: Test
tests =
  Group
    "Value declarations"
    [Group "LHS" lhsTests, switchToRHS (Group "RHS" rhsTests)]

lhsTests :: [Test]
lhsTests =
  [ Test "Backspace no-op" typeBackspace initState
  , Test "Type valid name" (typeChars "foo") (makeState "foo" initExpression)
  , Test "Ignore invalid name (digits)" (typeChars "123") initState
  , Test "Ignore invalid name (upper)" (typeChars "F") initState
  , Test
      "Type valid name, delete characters"
      (typeChars "foo" <> typeBackspace <> typeBackspace)
      (makeState "f" initExpression)
  ]

rhsTests :: [Test]
rhsTests =
  [ Test "Tab to RHS" [] (rhsSelectedState initExpression)
  , Test "Backspace no-op" typeBackspace (rhsSelectedState initExpression)
  , Test
      "Parens"
      (typeChars "(")
      (focus
         (UUID "2")
         (rhsSelectedState
            (ParensExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"})))))
  , Group "If" ifTests
  , Group "Lambda" lambdaTests
  , Group
      "Case"
      caseTests
  , Group "Variables" variableTests
  , Group
      "Literals"
      literalTests
  , Group "Function application" functionApplicationTests
  , Group "Infix" infixTests
  ]

literalTests :: [Test]
literalTests =
  [ Test
      "Type integer literal"
      (typeChars "123")
      (rhsSelectedState
         (LiteralExpression
            (Label {labelUUID = starterExprUUID})
            (IntegerLiteral 123)))
  , Test
      "Type integer literal, invalid chars ignored"
      (typeChars "123abc")
      (rhsSelectedState
         (LiteralExpression
            (Label {labelUUID = starterExprUUID})
            (IntegerLiteral 123)))
  ]

caseTests :: [Test]
caseTests =
  [ Test
      "Case completion"
      (typeChars "case ")
      (focus
         (UUID "2")
         (rhsSelectedState
            (CaseExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"}))
               [ CaseAlt
                 { caseAltLabel = Label {labelUUID = UUID "3"}
                 , caseAltPattern =
                     WildcardPattern (Label {labelUUID = UUID "4"}) "_"
                 , caseAltExpression =
                     ConstantExpression
                       (Label {labelUUID = UUID "5"})
                       (Identifier {identifierString = "_"})
                 }
               ])))
  ]

lambdaTests :: [Test]
lambdaTests =
  [ Test
      "Lambda completion"
      (typeChars "\\")
      (focus
         (UUID "3")
         (rhsSelectedState
            (LambdaExpression
               (Label {labelUUID = UUID "1"})
               (Alternative
                { alternativeLabel = Label {labelUUID = UUID "2"}
                , alternativePatterns =
                    [WildcardPattern (Label {labelUUID = UUID "3"}) "_"]
                , alternativeExpression =
                    ConstantExpression
                      (Label {labelUUID = UUID "4"})
                      (Identifier {identifierString = "_"})
                }))))
  ]

ifTests :: [Test]
ifTests =
  [ Test
      "If completion"
      (typeChars "if ")
      (focus
         (UUID "2")
         (rhsSelectedState
            (IfExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "3"})
                  (Identifier {identifierString = "_"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "4"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "If deletion"
      (typeChars "if " <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  , Test
      "If deletion after then"
      (typeChars "if " <> typeRight <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  , Test
      "If deletion after else"
      (typeChars "if " <> typeRight <> typeRight <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  ]

functionApplicationTests :: [Test]
functionApplicationTests =
  [ Test
      "Function completion"
      (typeChars "f ")
      (focus
         (UUID "1")
         (rhsSelectedState
            (ApplicationExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = starterExprUUID})
                  (Identifier {identifierString = "f"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Function completion (2 args)"
      (typeChars "f x y")
      (focus
         (UUID "5")
         (rhsSelectedState
            (ApplicationExpression
               (Label {labelUUID = UUID "6"})
               (ApplicationExpression
                  (Label {labelUUID = UUID "2"})
                  (VariableExpression
                     (Label {labelUUID = starterExprUUID})
                     (Identifier {identifierString = "f"}))
                  (VariableExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "x"})))
               (VariableExpression
                  (Label {labelUUID = UUID "5"})
                  (Identifier {identifierString = "y"})))))
  , Test
      "Type variable argument"
      (typeChars "f x")
      (focus
         (UUID "1")
         (rhsSelectedState
            (ApplicationExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = starterExprUUID})
                  (Identifier {identifierString = "f"}))
               (VariableExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "x"})))))
  , Test
      "Delete argument"
      (typeChars "f x" <> typeBackspace <> typeBackspace)
      (focus
         starterExprUUID
         (rhsSelectedState
            (VariableExpression
               (Label {labelUUID = starterExprUUID})
               (Identifier {identifierString = "f"}))))
  , Test
      "Delete function"
      (typeChars "f x" <> typeLeft <> typeBackspace <> typeBackspace)
      (focus
         (UUID "1")
         (rhsSelectedState
            (VariableExpression
               (Label {labelUUID = UUID "1"})
               (Identifier {identifierString = "x"}))))
  , Test
      "Add argument inbetween func and arg"
      (typeChars "f x" <> typeLeft <> typeChars " ")
      (focus
         (UUID "3")
         (rhsSelectedState
            (ApplicationExpression
               (Label {labelUUID = UUID "2"})
               (ApplicationExpression
                  (Label {labelUUID = UUID "4"})
                  (VariableExpression
                     (Label {labelUUID = starterExprUUID})
                     (Identifier {identifierString = "f"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "3"})
                     (Identifier {identifierString = "_"})))
               (VariableExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "x"})))))
  ]

infixTests :: [Test]
infixTests =
  [ Test
      "Infix completion (after hole)"
      (typeChars "*")
      (focus
         (UUID "1")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (ConstantExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "_"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after name)"
      (typeChars "x*")
      (focus
         (UUID "1")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after function application of hole)"
      (typeChars "x *")
      (focus
         (UUID "3")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "4"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "5"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "3"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after nested function application of hole)"
      (typeChars "x*y *")
      (focus
         (UUID "6")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (InfixExpression
                  (Label {labelUUID = UUID "7"})
                  (VariableExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "y"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "8"})
                      (Identifier {identifierString = "*"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "6"})
                     (Identifier {identifierString = "_"}))))))
  , Test
      "Infix completion (after nested function application of hole)"
      (typeChars "x*y +")
      (focus
         (UUID "6")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "7"})
               (InfixExpression
                  (Label {labelUUID = UUID "2"})
                  (VariableExpression
                     (Label {labelUUID = UUID "STARTER-EXPR"})
                     (Identifier {identifierString = "x"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "3"})
                      (Identifier {identifierString = "*"}))
                  (VariableExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "y"})))
               ( "+"
               , VariableExpression
                   (Label {labelUUID = UUID "8"})
                   (Identifier {identifierString = "+"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "6"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix preserves precedence"
      (typeChars "*+/")
      (focus
         (UUID "7")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "5"})
               (InfixExpression
                  (Label {labelUUID = UUID "2"})
                  (ConstantExpression
                     (Label {labelUUID = UUID "STARTER-EXPR"})
                     (Identifier {identifierString = "_"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "3"})
                      (Identifier {identifierString = "*"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "_"})))
               ( "+"
               , VariableExpression
                   (Label {labelUUID = UUID "6"})
                   (Identifier {identifierString = "+"}))
               (InfixExpression
                  (Label {labelUUID = UUID "8"})
                  (ConstantExpression
                     (Label {labelUUID = UUID "4"})
                     (Identifier {identifierString = "_"}))
                  ( "/"
                  , VariableExpression
                      (Label {labelUUID = UUID "9"})
                      (Identifier {identifierString = "/"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "7"})
                     (Identifier {identifierString = "_"}))))))
  ]

variableTests :: [Test]
variableTests =
  [ Test
      "Type variable name"
      (typeChars "foo")
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "foo"})))
  , Test
      "Ignore uppercase beginning"
      (typeChars "Foo")
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "oo"})))
  , Test
      "Type variable name, delete characters"
      (typeChars "foo" <> typeBackspace <> typeBackspace)
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "f"})))
  , Test
      "Type variable name, empty back to constant"
      (typeChars "foo" <> typeBackspace <> typeBackspace <> typeBackspace)
      (rhsSelectedState
         (ConstantExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "_"})))
  , Test
      "Type variable name with digits"
      (typeChars "foo123")
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "foo123"})))
  ]
