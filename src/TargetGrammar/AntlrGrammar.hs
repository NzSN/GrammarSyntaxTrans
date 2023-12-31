{-# LANGUAGE  LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- Utility to transform Rule into format that able to
-- parsed by Antlr4.
module TargetGrammar.AntlrGrammar (AntlrRepl(..), display) where

import qualified Data.Bifunctor as BiF
import qualified Data.Set as S
import qualified Data.List as List
import Data.Function
import Data.Char
import Control.Arrow
import GrammarParser (
  Rule(..),
  RuleExpr(..),
  Qualifier(..),
  ruleTraverse,
  mapRule)

newtype AntlrRepl = AntlrRepl { rules :: [Rule] }

data SemanticErrors =
  TerminalWithLowercase |
  InvalidCharInRule
  deriving (Eq, Show)

data RuleNeedFixed = RNF {
  rnfRule :: !Rule,
  rnfType :: !SemanticErrors }
  deriving (Eq, Show)

data RSemanticFix = RSF { rsfRules :: ![Rule],
                          rsfRNF   :: ![RuleNeedFixed] }
                  | RSF_DONE { rsfRules :: [Rule] }
                  deriving (Eq, Show)

data RSemanticFixer = RSFER { predicate :: !(SemanticErrors -> Bool),
                              fixing       :: !(Rule -> Rule -> Rule) }
                    | RSFER_ZERO

instance Semigroup RSemanticFixer where
  RSFER_ZERO <> f = f
  f <> RSFER_ZERO = f
  f <> f' = RSFER
              -- Chaining predicate
              (\x -> predicate f x || predicate f' x) $
              \r1 r2 ->
                -- Apply fixer f to rules
                fixing f r1 r2 &
                if r1 == r2
                then \r -> fixing f' r r
                else \r -> fixing f' r r2
instance Monoid RSemanticFixer where
  mempty = RSFER_ZERO
  mconcat [] = RSFER_ZERO
  mconcat (f:fs) = f <> mconcat fs

doFix :: [RSemanticFixer] -> RSemanticFix -> RSemanticFix
doFix _ (RSF_DONE rs) = RSF_DONE rs
doFix fixers (RSF rs rnfs) =
  RSF_DONE applyAllFixer
  where
    applyAllFixer :: [Rule]
    applyAllFixer =
      let -- Collapse RuleNeedFixed to a set that Cardinality equal to
          -- the set of problem Rules.
          problemRules = (S.toList . S.fromList $ [rnfRule a | a <- rnfs]) &
                         flip collapseRNF rnfs
          -- Mapping from [(Rule, [SemanticErrors])] to [(Rule, RSemanticFixer)]
          chainedFixers = [BiF.second chainFixer r | r <- problemRules]
      in applyToRules rs chainedFixers

      where
        applyToRules :: [Rule] -> [(Rule, RSemanticFixer)] -> [Rule]
        applyToRules rs [] = rs
        applyToRules rs' ((r', fixer):fixers') =
          applyToRules (map (\r'' -> fixing fixer r'' r') rs') fixers'

        collapseRNF :: [Rule] -> [RuleNeedFixed] -> [(Rule, [SemanticErrors])]
        collapseRNF [] _ = []
        collapseRNF (r:rs) rnfs =
          (r, map rnfType $ filter (\x -> rnfRule x == r) rnfs) :
          collapseRNF rs rnfs

        chainFixer :: [SemanticErrors] -> RSemanticFixer
        chainFixer [] = RSFER_ZERO
        chainFixer (e:errors) =
          case List.find (`predicate` e) fixers of
            Nothing    -> RSFER_ZERO
            Just fixer -> fixer <> chainFixer errors

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
        _                      -> ""

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
      disobedientTerms
      -- Fix differences
      >>> toAntlrTerminalForm

      where
        disobedientTerms :: [Rule] -> RSemanticFix
        disobedientTerms rs =
          RSF rs $
            (flip RNF TerminalWithLowercase <$>
               filter (\x -> isTerminal x && not (isUpper (head $ name x))) rs)
              <>
              -- Currently '.' is the only invalid character we want to find out.
              (flip RNF InvalidCharInRule <$> filter (elem '.' . name) rs)

        toAntlrTerminalForm :: RSemanticFix -> [Rule]
        toAntlrTerminalForm rsf =
          case doFix registerfixers rsf of
            -- Failed to fixed
            RSF _ _    -> []
            RSF_DONE rs -> rs


-------------------------------------------------------------------------------
--                            Definition of Fixers                           --
-------------------------------------------------------------------------------

-- Register your fixer to this list.
registerfixers :: [RSemanticFixer]
registerfixers = [
  terminalToUpperCase,
  removeDotInName
  ]

updateRefs :: String -> (RuleExpr -> RuleExpr) -> RuleExpr -> RuleExpr
updateRefs n trans re =
  case re of
    SubExpr n' _ -> if n' == n
                       -- Uppercase the first character
                    then trans re
                    else re
    _            -> re

-- Remove dot in rule name by replacing all
-- '.' by '_'
removeDotInName :: RSemanticFixer
removeDotInName =
    RSFER (== InvalidCharInRule) $
    \r r' ->
      let referDisobeidient =
            let ret = ruleTraverse r $
                  \case
                    SubExpr refName _ -> [refName == name r']
                    _                 -> [False]
            in maybe False or ret

          fixedRule
            | referDisobeidient =
                mapRule (updateRefs (name r') noDotTrans) r
            | name r == name r' = r { name = noDot $ name r }
            | otherwise = r
      in fixedRule
  where
    noDot :: String -> String
    noDot = map $ \c -> if c == '.' then '_' else c

    noDotTrans :: RuleExpr -> RuleExpr
    noDotTrans r =
      case r of
        SubExpr n q -> SubExpr (noDot n) q
        _           -> r

-- Uppercase first character of terminal rule
terminalToUpperCase :: RSemanticFixer
terminalToUpperCase =
  RSFER (== TerminalWithLowercase) $
  \r r' ->
  -- Traverse the rule to check that whether
  -- it's referece to the disobeydient rule
  let ruleName = name r'
      referDisobeidient =
        let ret = ruleTraverse r $
                    \case
                      SubExpr refName _  -> [refName == ruleName]
                      _                  -> [False]
        in maybe False or ret
      currentRule
          -- Uppercase all references
        | referDisobeidient  =
            mapRule (updateRefs ruleName antlrTermNameTrans) r
        | name r == ruleName = r { name = toAntlrTermName $ name r}
        | otherwise = r
  in currentRule

  where
    antlrTermNameTrans :: RuleExpr -> RuleExpr
    antlrTermNameTrans r =
      case r of
        SubExpr n q -> SubExpr (toAntlrTermName n) q
        _           -> r

    toAntlrTermName :: String -> String
    toAntlrTermName name = toUpper (head name) : tail name


instance Show AntlrRepl where
  show = display
