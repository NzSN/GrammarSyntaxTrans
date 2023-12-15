{-# OPTIONS_GHC -Wno-partial-fields #-}

module GrammarParser (
  Rule(..),
  Qualifier(..),
  parseGrammar) where

import Text.Parsec
import Text.Parsec.String ( GenParser )
import Text.ParserCombinators.ReadP (skipSpaces)

data Qualifier = Asterisk     -- Occurs zero or more times
               | Plus         -- Occurs one or more times
               | QuestionMark -- Occurs zoer or one times
               deriving (Show, Eq)

-- Internal datastructures for grammar rules
-- that shown in grammarExample.txt
data Rule = Rule {
                identity :: !String,
                subRules :: ![Rule],
                -- A rule may be qualified by some operator
                -- for example,
                --   statements:
                --     statement *
                -- The qualifier * indicate that statements occurs
                -- zero or more times.
                qualifier :: !(Maybe Qualifier) }
          | RuleAlternate { rules :: ![Rule] }
          | Interior { ref :: !Rule }
          | Terminal { literal :: !String } deriving (Show, Eq)

grammarFile :: GenParser Char st Rule
grammarFile = do
  rs <- many1 rule

  if length rs == 1
    then return $ head rs
    else return $ RuleAlternate rs

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

  -- Matching rule body
  r <- descdentRules

  return $ Terminal ruleID

descdentRules :: GenParser Char st Rule
descdentRules = undefined

parseGrammar' :: String -> Either ParseError Rule
parseGrammar' = parse grammarFile "(unknown)"

parseGrammar :: String -> Maybe Rule
parseGrammar x = case parseGrammar' x of
                      Left _ -> Nothing
                      Right r -> Just r
