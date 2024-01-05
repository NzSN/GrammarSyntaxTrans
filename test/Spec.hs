import qualified GrammarParserSpec
import qualified AntlrGrammarSpec
import qualified CPPGenSpec

main :: IO ()
main = do
  GrammarParserSpec.spec
  AntlrGrammarSpec.spec
  CPPGenSpec.spec
