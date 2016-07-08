module Main where

import Data.List  (deleteBy, find, foldl', intercalate, intersect, nub, permutations, union)

import System.IO
import System.Directory (doesFileExist, makeAbsolute)

import Types
import Typechecker (typecheck)
import Evaluator   (eval, replace)
import Parser

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
    Context   -> case ts of 
      [] -> putStrLn "~> Context currently empty!" >> repl ts
      _  -> do putStrLn "\n~> Current Context:"
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
    (Expr e)  -> case typecheck ts e of
                   (Left  r) -> putStrLn ("Error: Typecheck failed!\n" ++  r) >> repl ts
                   (Right _) -> let f = foldl' (\k (n,s) -> (replace (make_disjoint k s) (Placeholder n)) k)
                                    d (x:y:zs) = if x == y then x else d (y:zs)
                                    o = d $ iterate ((flip f) ts) e 
                                 in (print (eval o)) >> repl ts
    (Let n e) -> case typecheck ts e of
       (Left r)  -> putStrLn ("Error: Typecheck failed!\n" ++  r) >> repl ts
       (Right _) -> do putStrLn ("~> Expression now in context under name " ++ n)
                       case find (\(n',_) -> n' == n) ts of
                         (Nothing) -> repl (ts ++ [(n,e)])
                         (Just _)  -> repl ((deleteBy (\(a,_) (u,_) -> a == u) (n,e) ts) ++ [(n,e)])
    (Check e) -> do case typecheck ts e of
                      (Left  err) -> putStrLn $ "Error: Typecheck failed!\n" ++ err
                      (Right tau) -> putStrLn $ "This expression has type "  ++ show tau 
                    repl ts
    Clear     -> putStrLn "~> Context cleared!"              >> repl []
    (Load fl) -> do afl <- makeAbsolute fl
                    ex  <- doesFileExist afl
                    if ex then do inh <- readFile afl
                                  case parseFile inh of
                                    (Left err)  -> putStrLn err >> repl ts
                                    (Right res) -> do let (foo,bar) = foldl' embed ([],[]) res
                                                      case bar of 
                                                        [] -> pure ()
                                                        xs -> do putStrLn "~> The following expressions had incorrect types:"
                                                                 putStrLn ("   " ++ (intercalate ", " xs))
                                                      putStrLn "~> File loaded!"
                                                      repl (ts ++ foo)
                          else putStrLn "~> That file does not exist!" >> repl ts
    NoParse   -> putStrLn "~> Error: Could not parse input!" >> repl ts

embed :: (Context, [String]) -> (String, Exp, Typ) -> (Context, [String])
embed (con,fs) (nm, ex, tp) = case typecheck con ex of 
  (Left _)   -> (con, fs ++ [nm])
  (Right sg) -> if sg == tp then (con ++ [(nm, ex)],fs) else (con, fs ++ [nm])

-- Returns a version of the second expression, that has no shared
-- variables with the first expression. This is to avoid capture.
make_disjoint :: Exp -> Exp -> Exp
make_disjoint e1 e2 = rep e2 vb fresh
  where

    rep :: Exp -> [Exp]-> [Exp] -> Exp
    rep e []     _      = e
    rep e _      []     = error "Sorry, I ran out of fresh variables!" 
    rep e (x:xs) (y:ys) = rep (replace y x e) xs ys

    v1 = vars' e1
    v2 = vars' e2
    vb = nub $ intersect v1 v2

    fresh :: [Exp]
    fresh = filter (not . (flip elem (union v1 v2))) [ Var p | p <- (permutations "abcdefghij")]

    vars' :: Exp -> [Exp]
    vars' = nub . vars
 
    vars :: Exp -> [Exp]
    vars (Placeholder _)     = []
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
