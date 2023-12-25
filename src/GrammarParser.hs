{-# OPTIONS_GHC -Wno-partial-fields #-}

module GrammarParser (
  Rule(..),
  Qualifier(..),
  RuleExpr(..),
  parseGrammar,
  ruleTraverse,
  mapRule) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String

-------------------------------------------------------------------------------
--                        Rule Internal Representation                       --
-------------------------------------------------------------------------------

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

data RuleExpr = RExpr { exprs        :: ![RuleExpr] }
              | SubExpr { exprName   :: !String,
                          qualifier  :: !(Maybe Qualifier) }
              | Literal { literalVal :: !String,
                          qualifier  :: !(Maybe Qualifier) }
              | Regex { regexVal     :: !String }
              | Group { groupExprs   :: ![RuleExpr],
                        qualifier    :: !(Maybe Qualifier) }
              | Vertical
              deriving (Show, Eq)

-- Internal datastructures for grammar rules
-- that shown in grammarExample.txt
data Rule = Rule {
                name       :: !String,
                expr       :: ![RuleExpr],
                isTerminal :: !Bool }
            deriving (Show, Eq)

mapRule :: (RuleExpr -> RuleExpr) -> Rule -> Rule
mapRule f r = r { expr = map (traverse f) (expr r) }
  where
    traverse :: (RuleExpr -> RuleExpr) -> RuleExpr -> RuleExpr
    traverse f' r' =
      case r' of
        RExpr es    -> RExpr $ map (traverse f) es
        SubExpr _ _ -> f' r'
        Literal _ _ -> f' r'
        Regex   _   -> f' r'
        Group es q  -> Group (map (traverse f) es) q
        Vertical    -> f' r'

ruleTraverse :: Monoid b => Rule -> (RuleExpr -> b) -> Maybe b
ruleTraverse r = doTraverse (expr r)
  where
    doTraverse :: Monoid b => [RuleExpr] -> (RuleExpr -> b) -> Maybe b
    -- The only meaning of Nothing here is traverse
    -- to bottom of RuleExpr.
    doTraverse [] _ = Nothing
    doTraverse (r':rs') f' =
      let remain = doTraverse rs' f'
      in case remain of
           Nothing -> ruleExprTraverse r' f'
           Just x  -> ruleExprTraverse r' f' >>=
                        \y -> Just $ y <> x

    ruleExprTraverse :: Monoid b => RuleExpr -> (RuleExpr -> b) -> Maybe b
    ruleExprTraverse r' f' =
      case r' of
        RExpr e   -> doTraverse e f'
        Group e _ -> doTraverse e f'
        _         -> Just $ f' r'

-------------------------------------------------------------------------------
--                             Grammar Definition                            --
-------------------------------------------------------------------------------
isTerminal' :: RuleExpr -> [Bool]
isTerminal' r =
  case r of
    SubExpr _ _ -> [False]
    Literal _ _ -> [True]
    Regex _     -> [True]
    Vertical    -> [True]
    _           -> [True]

grammarFile :: GenParser Char st [Rule]
grammarFile = do
  many1 rule

eol :: GenParser Char st Char
eol = try (char '\n')

spaces'' :: GenParser Char st Char
spaces'' = char ' ' <|> char '\t'

rule :: GenParser Char st Rule
rule = do
  skipMany (try spaces'')
  ruleID <- many1 (noneOf ":")

  -- Discard unused characters
  string ":"
    >> skipMany (try spaces'')
    >> skipMany (try eol)

  rule <- flip (Rule ruleID) True <$> ruleExprs

  -- Determine the type of rule
  let isTerm = foldl (&&) True $
        fromJust (ruleTraverse rule isTerminal')

  return $ rule { isTerminal = isTerm }

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
exprElems = exprElems'
  where
    exprElems' :: GenParser Char st [RuleExpr]
    exprElems' = do
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
    <|> try alterElem
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
  n <- many1 (noneOf " \t\n*?+()'`<>|")

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

alterElem :: GenParser Char st (Maybe RuleExpr)
alterElem = do
  vertical <- char '|' <|> parserZero

  case vertical of
    '|' -> return $ Just $ Vertical
    _   -> return Nothing


parseGrammar' :: String -> Either ParseError [Rule]
parseGrammar' = parse grammarFile "(unknown)"

parseGrammar :: String -> Maybe [Rule]
parseGrammar x = case parseGrammar' x of
                      Left _ -> Nothing
                      Right r -> Just r
