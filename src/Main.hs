module Main where

import System.IO

import Types
import Evaluator
import Parser

import Examples
import GCD

repl :: [(String, Exp)] -> IO ()
repl ts = do
  putStr "goedelT > "
  inp <- getLine
  case parseInput inp of
    Quit      -> pure ()
    Context   -> case ts of 
      [] -> putStrLn "Context currently empty!" >> repl ts
      _  -> do putStrLn "Current Context:\n"
               mapM_ (\(n,exp) -> putStrLn (n ++ " : \n" ++ n ++ " = " ++ show exp)) ts
               putStrLn "" 
    Help    -> do putStrLn "Some commands for this REPL:\n"
                  putStrLn ":quit         Quit the REPL, exit"
                  putStrLn ":help         Display this help"
                  putStrLn ":context      Show current context"
                  putStrLn ":clear        Clear current context"
                  putStrLn "\n Type \"let (str) = (exp)\" to move a definition to context."
                  putStrLn "Otherwise, all input is interpreted as expressions"
                  putStrLn "" >> repl ts
    (Expr e)  -> let exp = e 
                  in do case typeCheck [] exp of
                          (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++  err
                          (Right tau) -> print $ eval exp
                        repl ts
    (Let n e) -> repl ((n,e):ts) 
    Clear     -> putStrLn "~> Context cleared!"              >> repl []
    NoParse   -> putStrLn "~> Error: Could not parse input!" >> repl ts

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Gödel's System T."
  putStrLn "Type :help for a list of commands, :quit to quit."
  repl []
