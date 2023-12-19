-- Utility to transform Rule into format that able to
-- parsed by Antlr4.
module TargetGrammar.AntlrGrammar () where

import GrammarParser (Rule(..))

data AntlrRepr = AntlrRepr { rules :: [Rule] }

display :: AntlrRepr -> String
display = \x -> "re"


instance Show AntlrRepr where
  show = display
