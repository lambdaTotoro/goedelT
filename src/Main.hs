module Main where

import System.IO

import Types
import Typechecker
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
      [] -> putStrLn "~> Context currently empty!" >> repl ts
      _  -> do putStrLn "Current Context:\n"
               mapM_ (\(n,exp) -> putStrLn ( "  " ++ n ++ " = " ++ show exp)) ts
               putStrLn ""
               repl ts 
    Help    -> mapM_ putStrLn 
                ["Some commands for this REPL:\n",
                 ":typecheck  e  Find and show the type of term e, if it exists",
                 ":eval       e  Evaluate term e",
                 ":let n name e  Move a term e to context with name n\n",
                 ":quit          Quit the REPL, exit",
                 ":help          Display this help",
                 ":context       Show current context",
                 ":clear         Clear current context\n"] >> repl ts
    (Expr e)  -> let exp = e 
                  in do case typecheck exp of
                          (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++  err
                          (Right tau) -> print $ eval exp
                        repl ts
    (Let n e) -> repl ((n,e):ts)
    (Check e) -> do case typecheck e of
                      (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++ err
                      (Right tau) -> putStrLn $ "This expression has type "  ++ show tau 
                    repl ts
    Clear     -> putStrLn "~> Context cleared!"              >> repl []
    NoParse   -> putStrLn "~> Error: Could not parse input!" >> repl ts

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Gödel's System T."
  putStrLn "Type :help for a list of commands, :quit to quit."
  repl []
