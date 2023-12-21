{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import System.IO
import System.Console.CmdArgs

import GrammarParser (parseGrammar)
import TargetGrammar.AntlrGrammar (AntlrRepr(AntlrRepr))

data Arguments = Arguments { input  :: String,
                             output :: String,
                             debug  :: Bool}
                deriving (Show, Data, Typeable)

arguments = Arguments{input = "grammar.txt" &= help "Path of input grammar file",
                      output = "grammar_output.txt" &= help "Path of output grammar file",
                      debug = False &= help "Debug mode"}

main :: IO ()
main = do
  -- Parse commandline arguments
  args <- cmdArgs arguments

  h <- openFile (input args) ReadMode
  out <- openFile (output args) WriteMode

  grammar_contents <- hGetContents h

  let rules = parseGrammar grammar_contents
  case rules of
    Nothing -> error "Failed to parse input grammar file"
    Just rs -> do
      let ruleRepr = AntlrRepr rs

      if debug args
        then do
          out_rule <- openFile "debug.txt" WriteMode
          hPrint out_rule rs >> hClose out_rule
        else do
          hPrint out ruleRepr >> hClose out

  hClose h
