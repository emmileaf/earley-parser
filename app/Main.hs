module Main where

import System.IO (hFlush, stdout, putStrLn, hPutStrLn, hPutStr)
import Debug.Trace

import Parser.Core
import Parser.Earley
import Grammar.Core
import Grammar.GrammarAE
import Grammar.GrammarNSE

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

setup :: IO ()
setup  = do
            input <- prompt "> " >> getLine
            case input of
                "quit" -> return ()
                "AE" -> repl aeg
                "NSE" -> repl nseg
                _ -> do
                        putStrLn "Grammar not supported. Valid values: [AE, NSE]"
                        setup

repl :: Grammar -> IO ()
repl g = do
            putStrLn "Please enter an input to be parsed"
            input <- prompt "> " >> getLine
            case input of
                "quit" -> return ()
                _ -> do
                        result <- print (printParsed (stepParse (startParse g input)))
                        repl g

main :: IO ()
main = do putStrLn "Hello! Please specify a grammar out of the following: [AE, NSE]"
          setup
          putStrLn "GoodBye!"
