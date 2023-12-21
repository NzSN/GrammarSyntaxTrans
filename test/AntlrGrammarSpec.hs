module AntlrGrammarSpec where

import Test.Hspec
import GrammarParser (parseGrammar)
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
          noSpaces (show repr) `shouldBe`
            noSpaces "additive_operator: \n  '+'   \n| '-'   ;\n\n"

    it "Group" $ do
      let sourceCode =
            "argument_expression_list: `'('` ( expression ( `','` expression )* `','` ? )? `')'`"
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepr r
          noSpaces (show repr) `shouldBe`
            noSpaces "argument_expression_list:'(' (expression  (','  expression )* ','? )? ')'  ;\n\n"
  where
    noSpaces :: String -> String
    noSpaces = filter (\x -> x /= ' ' && x /= '\n')
