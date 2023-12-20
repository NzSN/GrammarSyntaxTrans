{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import System.Console.CmdArgs

data Arguments = Arguments { input :: String,
                             output :: String }
                deriving (Show, Data, Typeable)

arguments = Arguments{input = "grammar.txt" &= help "Path of input grammar file",
                      output = "grammar_output.txt" &= help "Path of output grammar file"}

main :: IO ()
main = do
  -- Parse commandline arguments
  args <- cmdArgs arguments
