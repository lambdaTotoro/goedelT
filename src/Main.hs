module Main where

import Data.List (find, intersect, nub, union)
import System.IO

import Types
import Typechecker
import Evaluator
import Parser

-- TODO: Improve typechecker resp. option types and more (ℕ)?

-- Entry point of program
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Gödel's System T."
  putStrLn "Type :help for a list of commands, :quit to quit."
  repl []

-- Read - Evaluate - Print - Loop (REPL) for the language
repl :: Context -> IO ()
repl ts = do
  putStr "goedelT > "
  inp <- getLine
  case parseInput inp of
    Quit      -> pure ()
    Run       -> case find (\(a,_) -> a == "main") ts of
      (Just (a,e)) -> print (eval e) >> repl ts
      Nothing      -> putStrLn "~> No expression \"main\" currently in context." >> repl ts
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
                 ":let n name e  Move a term e to context with name n",
                 ":run           Evaluate term \"main\", if in context\n",
                 ":quit          Quit the REPL, exit",
                 ":help          Display this help",
                 ":context       Show current context",
                 ":clear         Clear current context\n"] >> repl ts
    (Expr e)  -> let exp = e 
                  in do case typecheck exp of
                          (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++  err
                          (Right tau) -> do print $ eval exp
                        repl ts
    (Let n e) -> repl ((n,e):ts)
    (Check e) -> do case typecheck e of
                      (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++ err
                      (Right tau) -> putStrLn $ "This expression has type "  ++ show tau 
                    repl ts
    Clear     -> putStrLn "~> Context cleared!"              >> repl []
    NoParse   -> putStrLn "~> Error: Could not parse input!" >> repl ts

-- Returns a vesion of the second expression, that has no shared
-- variables with the first expression. This is to avoid capture.
make_disjoint_to :: Exp -> Exp -> Exp
make_disjoint_to e1 e2 = rep e2 vb fresh
  where

    rep :: Exp -> [Exp]-> [Exp] -> Exp
    rep e []     _      = e
    rep e _      []     = error "Sorry, I ran out of fresh variables!" 
    rep e (x:xs) (y:ys) = rep (replace y x e) xs ys

    v1 = vars' e1
    v2 = vars' e2
    vb = nub $ intersect v1 v2

    fresh :: [Exp]
    fresh = filter (not . (flip elem (union v1 v2))) [(Var x)| x <- ['a'..'z']] ++ [(Var y)| y <- ['A'..'Z']]

    vars' :: Exp -> [Exp]
    vars' = nub . vars
 
    vars :: Exp -> [Exp]
    vars Z                   = []
    vars (Succ e)            = vars e
    vars (Var c)             = [(Var c)]
    vars (Lambda _ x e)      = x:(vars e)
    vars (Ap e1 e2)          = vars e1 ++ vars e2
    vars (Rec e0 x y e1 e)   = x:y:(vars e0 ++ vars e1 ++ vars e)
    vars Truth               = []
    vars Falsehood           = []
    vars (If t bt bf)        = vars t ++ vars bt ++ vars bf
    vars (Empty _)           = []
    vars (Full e)            = vars e
    vars (Which _ e0 x e1 e) = x:vars e0 ++ vars e1 ++ vars e
    vars Triv                = []
    vars (Tuple e1 e2)       = vars e1 ++ vars e2
    vars (Pi_one e)          = vars e
    vars (Pi_two e)          = vars e
    vars (Abort _ e)         = vars e
    vars (InL _ _ e)         = vars e
    vars (InR _ _ e)         = vars e
    vars (Case e x e1 y e2)  = x:y:vars e1 ++ vars e2
