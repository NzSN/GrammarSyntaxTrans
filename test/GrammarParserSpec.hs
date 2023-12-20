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

    it "Rule with Regex" $ do
      let sourceCode =
            "swizzle_name: \n\
            \| `/[rgba]/` \n\
            \| `/[rgba][rgba]/` \n\
            \| `/[rgba][rgba][rgba]/` \n\
            \| `/[rgba][rgba][rgba][rgba]/`"
          rule = parseGrammar sourceCode
          expectRules = [Rule "swizzle_name"
                         [RExpr [Regex "[rgba]"],
                          RExpr [Regex "[rgba][rgba]"],
                          RExpr [Regex "[rgba][rgba][rgba]"],
                          RExpr [Regex "[rgba][rgba][rgba][rgba]"]
                          ]]
      case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` expectRules

    it "Rule with Regex and Paren" $ do
      let sourceCode =
            "template_elaborated_ident.post.ident: \n\
            \| ( _template_args_start template_arg_expression ( `','` expression )* `','` ? _template_args_end )?"
          rule = parseGrammar sourceCode
          expectRules = [
            Rule "template_elaborated_ident.post.ident"
                [RExpr [Group [SubExpr "_template_args_start" Nothing,
                               SubExpr "template_arg_expression" Nothing,
                               Group [Literal "," Nothing,
                                      SubExpr "expression" Nothing] $ Just Asterisk,
                               Literal "," $ Just QuestionMark,
                               SubExpr "_template_args_end" Nothing
                              ] $ Just QuestionMark]
                ]
            ]

      case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` expectRules

    it "Rule with alternative ( | )" $ do
      let sourceCode = "variable_updating_statement:  \n\
                       \| lhs_expression ( `'='` | compound_assignment_operator ) expression"
          rule = parseGrammar sourceCode
          expectRules = [Rule "variable_updating_statement"
                         [RExpr [SubExpr "lhs_expression" Nothing,
                                Group [Literal "=" Nothing, Vertical, SubExpr "compound_assignment_operator" Nothing] Nothing,
                                SubExpr "expression" Nothing
                                ]
                         ]
                        ]

      case rule of
         Nothing -> rule `shouldNotBe` Nothing
         Just r -> r `shouldBe` expectRules
