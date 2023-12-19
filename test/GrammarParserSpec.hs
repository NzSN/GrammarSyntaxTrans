module GrammarParserSpec (spec) where

import Test.Hspec
import GrammarParser (parseGrammar, Rule(..), RuleExpr(..), Qualifier(..))

spec :: IO ()
spec = do
  spec_intro

spec_intro :: IO ()
spec_intro = hspec $ do
  describe "Basic cases" $ do
    it "Single Rule (single rule expr)" $ do
       let sourceCode = "additive_operator: \n `'+'`"
           rule = parseGrammar sourceCode
           expectRule = Rule "additive_operator"
                        [RExpr [Literal "+" Nothing]]

       case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` [expectRule]

    it "Single alternative Rule" $ do
      let sourceCode = "additive_operator: \n\
                       \| `'-'`            \n\
                       \| `'+'`"
          rule = parseGrammar sourceCode
          expectRule = Rule "additive_operator"
                       [RExpr [Literal "-" Nothing],
                        RExpr [Literal "+" Nothing]]

      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> r `shouldBe` [expectRule]

    it "Multiple Rule" $ do
      let sourceCode = "arith_operator:    \n\
                       \| `'-'`            \n\
                       \| `'+'`            \n\
                                           \n\
                       \equality_operator: \n\
                       \| `'<'`            \n\
                       \| `'>'`            \n\
                       \| `'>='`           \n\
                       \| `'<='`           \n\
                       \PI: `'3.1415'`"

          rule = parseGrammar sourceCode
          expectRules = [Rule "arith_operator"
                         [RExpr [Literal "-" Nothing],
                          RExpr [Literal "+" Nothing]],
                         Rule "nequality_operator"
                         [RExpr [Literal "<" Nothing],
                          RExpr [Literal ">" Nothing],
                          RExpr [Literal ">=" Nothing],
                          RExpr [Literal "<=" Nothing]],
                         Rule "PI" [RExpr [Literal "3.1415" Nothing]]
                        ]
      case rule of
        Nothing -> rule `shouldNotBe` Nothing
        Just r -> r `shouldBe` expectRules

    it "Rule contain interior" $ do
       let sourceCode = "arith_operator: \n\
                         \| plus         \n\
                         \| minus        \n\
                         \| mul          \n\
                         \| div"
           rule = parseGrammar sourceCode
           expectRules = [Rule "arith_operator"
                            [RExpr [SubExpr "plus" Nothing],
                             RExpr [SubExpr "minus" Nothing],
                             RExpr [SubExpr "mul" Nothing],
                             RExpr [SubExpr "div" Nothing]]]

       case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` expectRules

    it "Rule with qualifier" $ do
      let sourceCode = "stmts: stmt*           \n\
                       \atLeastOneStmts: stmt+ \n\
                       \zeroOrOneStmts: stmt  ?"
          rule = parseGrammar sourceCode
          expectRules = [-- stmts
                         Rule "stmts"
                         [RExpr [SubExpr "stmt" $ Just Asterisk]],
                         -- atLeastOneStmts
                         Rule "atLeastOneStmts"
                         [RExpr [SubExpr "stmt" $ Just Plus]],
                         -- zeroOrOneStmts
                         Rule "zeroOrOneStmts"
                         [RExpr [SubExpr "stmt" $ Just QuestionMark]]]
      case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` expectRules
