-- Utility to transform Rule into format that able to
-- parsed by Antlr4.
module TargetGrammar.AntlrGrammar (AntlrRepr(..), display) where

import GrammarParser (Rule(..), RuleExpr(..), Qualifier(..))

newtype AntlrRepr = AntlrRepr { rules :: [Rule] }

ruleSep :: String
ruleSep = ";\n\n"

display :: AntlrRepr -> String
display ar = display' $ rules ar
  where
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
        Regex val              -> "'/" ++ val ++ "/'"
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

instance Show AntlrRepr where
  show = display
