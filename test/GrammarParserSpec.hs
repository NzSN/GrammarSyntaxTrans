module GrammarParserSpec (spec) where

import Test.Hspec
import GrammarParser (parseGrammar, Rule(..))

spec :: IO ()
spec = do
  spec_intro

spec_intro :: IO ()
spec_intro = hspec $ do
  describe "GrammarParser Introduction" $ do
    it "Parse" $ do
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> r `shouldBe` expectRule
    where
      sourceCode = "additive_operator: \n\
                    \| `'+'`           \n\
                    \| `'-'`"
      rule = parseGrammar sourceCode
      expectRule = Rule "additive_operator"
                        [Terminal "+", Terminal "-"]
                        Nothing
