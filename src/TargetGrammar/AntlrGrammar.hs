-- Utility to transform Rule into format that able to
-- parsed by Antlr4.
module TargetGrammar.AntlrGrammar (AntlrRepl(..), display) where

import Data.Char
import Control.Arrow
import GrammarParser (Rule(..), RuleExpr(..), Qualifier(..))

newtype AntlrRepl = AntlrRepl { rules :: [Rule] }

ruleSep :: String
ruleSep = ";\n\n"

display :: AntlrRepl -> String
display =
  -- Extrac rules from AntlrRepl
  rules >>>
  -- Semantic of Antlr is different from
  -- is different from grammar defined in
  -- Standard, need to correct such differences.
  semanticCorrect >>>
  -- Syntax transformation is able to perform
  -- after correctness of semantic.
  display'

  where
    -- Syntax-level transformation
    display' :: [Rule] -> String
    display' (r:rs) =
      let r_name = name r ++ ": "
          r_body = if length (expr r) == 1
                     then displayRuleExpr $ expr r
                          -- Manually deal with the first rule expression
                     else "\n  " ++ displayRuleExpr [head $ expr r] ++
                          displayRuleExprs (tail $ expr r)
      in r_name ++ r_body ++ ruleSep ++ display' rs
    display' [] = ""

    -- Alternatives
    displayRuleExprs :: [RuleExpr] -> String
    displayRuleExprs (r:rs) =
      let r_str = displayRuleExpr [r]
      in  "\n| " ++ r_str ++ displayRuleExprs rs
    displayRuleExprs [] = ""

    displayRuleExpr :: [RuleExpr] -> String
    displayRuleExpr [] = ""
    displayRuleExpr (r:rs) = do
      case r of
        RExpr (r':rs')           ->
          displayRuleExpr [r'] ++ " " ++ displayRuleExpr rs'
        SubExpr name qualifier ->
          case qualifier of
            Just Asterisk     -> name ++ "*"
            Just Plus         -> name ++ "+"
            Just QuestionMark -> name ++ "?"
            Nothing           -> name
        Literal val qualifier  ->
          case qualifier of
            Just Asterisk     -> "'" ++ val ++ "'" ++ "*"
            Just Plus         -> "'" ++ val ++ "'" ++ "+"
            Just QuestionMark -> "'" ++ val ++ "'" ++ "?"
            Nothing           -> "'" ++ val ++ "'"
        Regex val              -> val
        Group (r':rs') qualifier ->
          "("
            ++ displayRuleExpr [r']
            ++ " "
            ++ displayRuleExpr rs'
            ++ ")"
            ++ appendQualifier qualifier
          where
            appendQualifier :: Maybe Qualifier -> String
            appendQualifier q = do
              case q of
                Just Asterisk     -> "*"
                Just Plus         -> "+"
                Just QuestionMark -> "?"
                Nothing           -> ""
        Vertical               -> "|"

        ++ " " ++ displayRuleExpr rs

    -- Semantic-level transformation
    semanticCorrect :: [Rule] -> [Rule]
    semanticCorrect = termDiffCorrect

    -- In W3C Draft, the name of a rule that represent a terminal
    -- is consist of lowercase characters. But first character of
    -- terminal name is force to upper case.
    termDiffCorrect :: [Rule] -> [Rule]
    termDiffCorrect =
      -- Find out terminal rule that disobey semantic
      -- of Antlr syntax.
      (\rs -> (rs :: [Rule], disobedientTerms rs))
      >>> toAntlrTerminalForm

      where
        disobedientTerms :: [Rule] -> [Rule]
        disobedientTerms =
          filter $ \x -> isTerminal x && isUpper (head $ name x)

        toAntlrTerminalForm :: ([Rule],[Rule]) -> [Rule]
        toAntlrTerminalForm =

instance Show AntlrRepl where
  show = display
