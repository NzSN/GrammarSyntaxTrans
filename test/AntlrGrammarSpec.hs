module AntlrGrammarSpec where

import Test.Hspec
import GrammarParser (parseGrammar, Rule(..), RuleExpr(..), Qualifier(..))
import TargetGrammar.AntlrGrammar (AntlrRepr(AntlrRepr))

spec :: IO ()
spec = do
  spec_intro


spec_intro :: IO ()
spec_intro = hspec $ do
  describe "Basic cases" $ do
    it "Single literal" $ do
      let sourceCode = "additive_operator:  \n\
                       \| `'+'` \n\
                       \| `'-'` "
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepr r
          print $ show repr
