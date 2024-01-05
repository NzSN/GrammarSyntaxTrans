module PLs.CPPCode (
  CPP(..)) where

import Data.Char
import Data.List

import GrammarParser (
  Rule(..),
  ruleTraverse')

type ArgType = String
type ArgName = String
data CPP = CPP { parser_name :: String,
                 entry_point :: String,
                 contextNamePostfix :: String,
                 -- For example, clone%RContext will result in
                 -- name with prefix 'clone' and postfix 'Context'
                 -- to rule name.
                 methodNameWildcard :: String,
                 initializers :: [String],
                 methodArgs :: [(ArgType, ArgName)],
                 rules :: [Rule] }

instance Show CPP where
  show = display

display :: CPP -> String
display cpp =
  mconcat $ map (ruleToCPP cpp) $ rules cpp
  ++
  -- Base functions
  -- Deep clone method of terminal and
  -- clone interface.

toCPPContextName :: CPP -> String -> String
toCPPContextName cpp s =
  parser_name cpp ++ "::" ++
  (toUpper . head $ s) : tail s ++
  contextNamePostfix cpp


toCPPContextNamePtr :: CPP -> String -> String
toCPPContextNamePtr cpp s = toCPPContextName cpp s ++ "*"

toCPPMethodName :: CPP -> Rule -> String
toCPPMethodName cpp r = "MethodName"

argStrings :: CPP -> String
argStrings cpp =
  let s = foldl f "" $ methodArgs cpp
  in case s of
    "" -> mempty
    _  -> init s
  where
    f :: String -> (ArgType, ArgName) -> String
    f s (t,n) = s ++ t ++ " " ++ n ++ ","

-- Currently, the only infromation required
-- is the name of rule.
ruleToCPP :: CPP -> Rule -> String
ruleToCPP cpp r =
  -- Generate deep clone codes for Antlr TreeNodes
  if isTerminal r
  then ""
  else
    -- Return type of clone methods
    ctxPtr ++ "\n" ++
    -- Clone method name
    toCPPMethodName cpp r ++
    -- Arguments
    "(" ++ ctxPtr ++ ",\n" ++ argStrings cpp ++ ") {\n"
    ++ impl cpp r ++
    "}"
  where
    ctxPtr = toCPPContextNamePtr cpp (name r)
    ctx = toCPPContextName cpp (name r)

    impl :: CPP -> Rule -> String
    impl cpp r
      | entry_point cpp == name r =
                ctxPtr ++ "copy" ++ "\n    " ++
                "new " ++ ctx ++ "(nullptr, -1);\n" ++
                "return copy"
      | otherwise =
                ctxPtr ++ " copy" ++ "\n    " ++
                "new " ++ ctx ++
                "(\n" ++
                "dynamic_cast<" ++ ctxPtr ++ ">(\n" ++
                (init . foldl (\s s' -> s ++ s' ++ ",") "" $ initializers cpp)
                ++ ");\n"
                ++ "  return copy;"
