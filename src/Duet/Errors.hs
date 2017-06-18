{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Errors where

import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Ord
import           Duet.Printer
import           Duet.Types
import           Text.EditDistance

displayResolveException :: SpecialTypes Name -> ResolveException -> String
displayResolveException specialTypes =
  \case
    NoInstanceFor p -> "No instance for " ++ printPredicate defaultPrint specialTypes p

displayStepperException :: a -> StepException -> String
displayStepperException _ =
  \case
    CouldntFindName n -> "Not in scope: " ++ curlyQuotes (printit defaultPrint n)
    CouldntFindNameByString n ->
      "The starter variable isn't defined: " ++
      curlyQuotes n ++
      "\nPlease define a variable called " ++ curlyQuotes n
    TypeAtValueScope k -> "Type at value scope: " ++ show k

displayInferException :: SpecialTypes Name -> InferException -> [Char]
displayInferException specialTypes =
  \case
    ExplicitTypeMismatch sc1 sc2 ->
      "The type of a definition doesn't match its explicit type:\n\n  " ++
     printScheme defaultPrint specialTypes sc1 ++ "\n\nand\n\n  " ++

     printScheme defaultPrint specialTypes sc2 ++ "\n\n" ++
       show sc1 ++ "\n" ++ show sc2
    NotInScope scope name ->
      "Not in scope " ++
      curlyQuotes (printit defaultPrint name) ++
      "\n" ++
      "Nearest names in scope:\n\n" ++
      intercalate
        ", "
        (map
           curlyQuotes
           (take
              5
              (sortBy
                 (comparing (editDistance (printit defaultPrint name)))
                 (map (printTypeSignature defaultPrint specialTypes) scope))))
    TypeMismatch t1 t2 ->
      "Couldn't match type " ++
      curlyQuotes (printType defaultPrint specialTypes t1) ++
      "\n" ++
      "against inferred type " ++ curlyQuotes (printType defaultPrint specialTypes t2)
    OccursCheckFails ->
      "Infinite type (occurs check failed). \nYou \
                        \probably have a self-referential value!"
    AmbiguousInstance ambiguities ->
      "Couldn't infer which instances to use for\n" ++
      unlines
        (map
           (\(Ambiguity _ ps) ->
              intercalate ", " (map (printPredicate defaultPrint specialTypes) ps))
           ambiguities)
    e -> show e

displayRenamerException :: SpecialTypes Name -> RenamerException -> [Char]
displayRenamerException specialTypes =
  wrap (\case
          IdentifierNotInVarScope scope name ->
            "Not in variable scope " ++
            curlyQuotes (printit defaultPrint name) ++
            "\n" ++
            "Nearest names in scope:\n\n" ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printit defaultPrint name)))
                       (map (printit defaultPrint) (M.elems scope)))))
          IdentifierNotInConScope scope name ->
            "Not in constructors scope " ++
            curlyQuotes (printit defaultPrint name) ++
            "\n" ++
            "Nearest names in scope:\n\n" ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printit defaultPrint name)))
                       (map (printit defaultPrint) (M.elems scope)))))
          KindTooManyArgs ty k ty2 ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes ty ++ " :: " ++ printKind k) ++
            " has an unexpected additional argument, " ++
            curlyQuotes (printType defaultPrint specialTypes ty2)
          ConstructorFieldKind cons typ kind ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes typ ++ " :: " ++ printKind kind) ++
            " is used in a field in the " ++
            curlyQuotes (printit defaultPrint cons) ++
            " constructor, but all fields \
            \should have types of kind " ++
            curlyQuotes (printKind StarKind)
          KindArgMismatch t1 k1 t2 k2 ->
            "The type " ++
            curlyQuotes (printType defaultPrint specialTypes t1 ++ " :: " ++ printKind k1) ++
            " has been given an argument of the wrong kind " ++
            curlyQuotes (printType defaultPrint specialTypes t2 ++ " :: " ++ printKind k2)
          TypeNotInScope types i ->
            "Unknown type " ++
            curlyQuotes (printIdentifier defaultPrint i) ++
            "\n" ++
            "Closest names in scope are: " ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (take
                    5
                    (sortBy
                       (comparing (editDistance (printIdentifier defaultPrint i)))
                       (map (printTypeConstructor defaultPrint) types))))
          UnknownTypeVariable types i ->
            "Unknown type variable " ++
            curlyQuotes (printIdentifier defaultPrint i) ++
            "\n" ++
            "Type variables in scope are: " ++
            intercalate
              ", "
              (map
                 curlyQuotes
                 (sortBy
                    (comparing (editDistance (printIdentifier defaultPrint i)))
                    (map (printTypeVariable defaultPrint) types)))
          e -> show e)
  where wrap f e = (f e)-- ++ "\n(" ++ show e ++ ")"

editDistance :: [Char] -> [Char] -> Int
editDistance = on (levenshteinDistance defaultEditCosts) (map toLower)
