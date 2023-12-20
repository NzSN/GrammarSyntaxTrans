import qualified GrammarParserSpec
import qualified AntlrGrammarSpec

main :: IO ()
main = do
  GrammarParserSpec.spec
  AntlrGrammarSpec.spec
