module AntlrGrammarSpec (spec) where

import Test.Hspec
import GrammarParser (parseGrammar)
import TargetGrammar.AntlrGrammar (AntlrRepl(AntlrRepl))

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
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "Additive_operator: \n  '+'   \n| '-'   ;\n\n"

    it "Group" $ do
      let sourceCode =
            "argument_expression_list: `'('` ( expression ( `','` expression )* `','` ? )? `')'`"
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "argument_expression_list:'(' (expression  (','  expression )* ','? )? ')'  ;\n\n"

    it "illTermWithRefs" $ do
      let sourceCode = "additive_operator:         \n\
                       \| `'+'`                    \n\
                       \| `'-'`                    \n\
                       \op: `'++'`                 \n\
                       \expr: additive_operator op \n\
                       \irrelevant: anotherExpr"
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "Additive_operator: \n  '+'   \n| '-'   ;\n\n Op: '++' ;\n\n expr: Additive_operator Op;\n\n irrelevant: anotherExpr;"

    it "RuleNameContainDots" $ do
      let sourceCode = "additive.operator:  \n\
                       \| `'+'` \n\
                       \| expression "
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "additive_operator: \n  '+'   \n| expression   ;\n\n"

    it "RuleNameContainDots_Multiple_Fixer" $ do
      let sourceCode = "additive.operator:  \n\
                       \| `'+'` \n\
                       \| `'-'` "
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "Additive_operator: \n  '+'   \n| '-'   ;\n\n"


    -- FIXME: Fixer still generate rule with semantic error
    --        cause a fixer may only fix the head underscore,
    --        which result in rule name in additive_operator.
    --        Cause detecter unable to detect semantic error
    --        during fixing.
    it "BeginWithUnderscore" $ do
      let sourceCode = "_additive.operator:  \n\
                       \| `'+'` \n\
                       \| `'-'` "
          rule = parseGrammar sourceCode
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> do
          let repr = AntlrRepl r
          noSpaces (show repr) `shouldBe`
            noSpaces "Additive_operator: \n  '+'   \n| '-'   ;\n\n"



  where
    noSpaces :: String -> String
    noSpaces = filter (\x -> x /= ' ' && x /= '\n')
