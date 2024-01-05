module CPPGenSpec (spec) where

import Test.Hspec
import GrammarParser (Rule(..))
import PLs.CPPCode (CPP(..))


spec :: IO ()
spec = hspec $ do
  describe "Basic case" $ do
    it "generate" $ do
      let rule = Rule { name = "RName", expr = [], isTerminal = False }
          cpp = CPP {
            parser_name = "WGSLParser",
            entry_point = "translation_unit",
            contextNamePostfix = "Context",
            methodNameWildcard = "clone%RContext",
            initializers =
                ["info.parent != nullptr ? info.parent->tree() : nullptr",
                 "node->invokingState"],
            methodArgs = [("TransformInfo<Antlr4Node>&", "info")],
            rules = [rule]
            }

      print (show cpp)
