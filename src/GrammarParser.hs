{-# OPTIONS_GHC -Wno-partial-fields #-}

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
                          qualifier :: !(Maybe Qualifier)
                        }
              | Literal { literalVal :: !String,
                          qualifier :: !(Maybe Qualifier) }
              deriving (Show, Eq)

-- Internal datastructures for grammar rules
-- that shown in grammarExample.txt
data Rule = Rule {
                name :: !String,
                expr :: ![RuleExpr] }
            deriving (Show, Eq)

grammarFile :: GenParser Char st [Rule]
grammarFile = many1 rule

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
ruleExprs = try alternativeExprs <|> singleExpr

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
  e <- try literalElem
       <|> try subExprElem
       <|> return Nothing

  return e

literalElem :: GenParser Char st (Maybe RuleExpr)
literalElem = do
  _ <- string "`'"
  literalStr <- many (noneOf "'`")
  _ <- string "'`"

  return $ Just $ Literal literalStr Nothing

subExprElem :: GenParser Char st (Maybe RuleExpr)
subExprElem = do
  n <- many1 (noneOf " \t\n")
  return $ Just $ SubExpr n Nothing

parseGrammar' :: String -> Either ParseError [Rule]
parseGrammar' = parse grammarFile "(unknown)"

parseGrammar :: String -> Maybe [Rule]
parseGrammar x = case parseGrammar' x of
                      Left _ -> Nothing
                      Right r -> Just r
