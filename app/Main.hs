{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Control.Exception

import System.IO
import System.Console.CmdArgs

import GrammarParser (parseGrammar, Rule)
import TargetGrammar.AntlrGrammar (AntlrRepl(AntlrRepl))

type Path = String
data Arguments = Arguments { input  :: !String,
                             output :: !String,
                             debug  :: !Bool}
                deriving (Show, Data, Typeable)

arguments :: Arguments
arguments = Arguments{input = "grammar.txt" &= help "Path of input grammar file",
                      output = "grammar_output.txt" &= help "Path of output grammar file",
                      debug = False &= help "Debug mode"}

main :: IO ()
main =
  -- Parse commandline arguments
  cmdArgs arguments >>= \args ->
    -- Parse input grammar file
    parseFile (input args) >>=
    -- Transform to Antlr form
    asAntlr (output args)
  where
    parseFile :: Path -> IO (Maybe [Rule])
    parseFile path = do
      withFile path ReadMode $ \h -> do
        contents <- hGetContents h
        let rules = parseGrammar contents
        case rules of
          Nothing -> return Nothing
          Just rs  -> return $ Just rs

    asAntlr :: Path -> Maybe [Rule] -> IO ()
    asAntlr path rsMaybe = do
      case rsMaybe of
        Nothing -> error "Nothing to transform (maybe fail to parse input file)"
        Just rs -> withFile path WriteMode
                    (\h -> hPrint h $ AntlrRepl rs)
