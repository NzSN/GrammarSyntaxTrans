{-# OPTIONS_GHC -Wno-partial-fields #-}

-- TODO: Support of parsing regex in grammar

module GrammarParser (
  Rule(..),
  Qualifier(..),
  RuleExpr(..),
  parseGrammar) where

import Debug.Trace (trace)
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

-- A rule may be qualified by some operator
-- for example,
--   statements:
--     statement *
-- The qualifier * indicate that statements occurs
-- zero or more times.
data Qualifier = Asterisk     -- Occurs zero or more times
               | Plus         -- Occurs one or more times
               | QuestionMark -- Occurs zoer or one times
               deriving (Show, Eq)

data RuleExpr = RExpr { exprs :: ![RuleExpr] }
              | SubExpr { exprName :: !String,
                          qualifier :: !(Maybe Qualifier) }
              | Literal { literalVal :: !String,
                          qualifier :: !(Maybe Qualifier) }
              | Regex { regexVal :: !String }
              | Group { groupExprs :: ![RuleExpr],
                        qualifier :: !(Maybe Qualifier) }
              deriving (Show, Eq)

-- Internal datastructures for grammar rules
-- that shown in grammarExample.txt
data Rule = Rule {
                name :: !String,
                expr :: ![RuleExpr] }
            deriving (Show, Eq)

grammarFile :: GenParser Char st [Rule]
grammarFile = do
  many1 rule

eol :: GenParser Char st Char
eol = try (char '\n')

spaces'' :: GenParser Char st Char
spaces'' = char ' ' <|> char '\t'

rule :: GenParser Char st Rule
rule = do

  ruleID <- many1 (noneOf ":")

  -- Discard unused characters
  string ":"
    >> skipMany (try spaces'')
    >> skipMany (try eol)

  Rule ruleID <$> ruleExprs

ruleExprs :: GenParser Char st [RuleExpr]
ruleExprs = do
  expr <- try alternativeExprs <|> singleExpr

  -- Gurantee that all characters of the parsed
  -- is consumed
  skipMany (try spaces'')
  skipMany (try eol)

  return expr

singleExpr :: GenParser Char st [RuleExpr]
singleExpr = do
  skipMany (try spaces'')
  skipMany (try eol)

  expr <- exprElems

  return [RExpr expr]

alternativeExprs :: GenParser Char st [RuleExpr]
alternativeExprs = do

  skipMany (try spaces'')
  _ <- char '|'

  expr <- exprElems

  if null expr
    then return []
    else do
      skipMany (try spaces'')
      skipMany (try eol)

      exprs <- try alternativeExprs <|>
               return []
      return $ RExpr expr : exprs

exprElems :: GenParser Char st [RuleExpr]
exprElems = do
  e <- exprElem

  if isNothing e
    -- No more elements
    then return []
    else do

      -- Spaces may exists between expression elements
      skipMany (try spaces'')

      remain <- exprElems

      return $ fromJust e : remain

exprElem :: GenParser Char st (Maybe RuleExpr)
exprElem = do
  skipMany (try spaces'')
  try literalElem
    <|> try subExprElem
    <|> try regexElem
    <|> try groupElem
    <|> return Nothing

literalElem :: GenParser Char st (Maybe RuleExpr)
literalElem = do
  _ <- string "`'"
  literalStr <- many (noneOf "'`")
  _ <- string "'`"

  _ <- skipMany (try spaces'')
  qualifier <- try qualifierElem <|>
               return Nothing

  return $ Just $ Literal literalStr qualifier

subExprElem :: GenParser Char st (Maybe RuleExpr)
subExprElem = do
  n <- many1 (noneOf " \t\n*?+()'`<>")

  _ <- skipMany (try spaces'')

  -- Qualifier may exists
  qualifier <- try qualifierElem <|>
               return Nothing

  return $ Just $ SubExpr n qualifier

regexElem :: GenParser Char st (Maybe RuleExpr)
regexElem = do
  -- Assume that there are no escapes
  _ <- string "`/"
  regex <- many (noneOf "/")
  _ <- string "/`"

  return $ Just $ Regex regex

groupElem :: GenParser Char st (Maybe RuleExpr)
groupElem = do
  _ <- char '('

  skipMany (try spaces'')

  exprs <- exprElems
  trace (show exprs) $ return Nothing

  skipMany (try spaces'')
  _ <- char ')'

  _ <- skipMany (try spaces'')

  qualifier <- try qualifierElem <|>
               return Nothing

  return $ Just $ Group exprs qualifier

qualifierElem :: GenParser Char st (Maybe Qualifier)
qualifierElem = do
  q <- try (char '*') <|>
       try (char '+') <|>
       try (char '?') <|>
       parserZero

  case q of
    '*' -> return $ Just Asterisk
    '+' -> return $ Just Plus
    '?' -> return $ Just QuestionMark
    _   -> return Nothing


parseGrammar' :: String -> Either ParseError [Rule]
parseGrammar' = parse grammarFile "(unknown)"

parseGrammar :: String -> Maybe [Rule]
parseGrammar x = case parseGrammar' x of
                      Left _ -> Nothing
                      Right r -> Just r
