module GCD where

import Types
import Evaluator
import Examples

reference_gcd :: Int -> Int -> Int
reference_gcd a b 
  | (b == 0)  = a
  | otherwise = reference_gcd b (a `mod` b)

test :: (Int -> Int -> Int) -> Exp -> Bool
test f exp = and [ (f k l) == nat2int (eval (Ap (Ap exp (int2nat k)) (int2nat l)))  | k <- [0..100], l <- [0..100]]

valids :: [Exp]
valids = filter (test (+)) (map fst typecorrects)

typecorrects :: [(Exp, Either String Typ)]
typecorrects = (filter (\(_,t) -> (t == Right (Arrow Nat (Arrow Nat Nat))))) foo 
  where
    foo :: [(Exp, Either String Typ)]
    foo = map (\e -> (e, typeCheck [] e)) possibles

possibles :: [Exp]
possibles = generate 2

-------------------------------------------

types :: [Typ]
types = [Nat, Arrow Nat Nat, Arrow Nat (Arrow Nat Nat), Arrow Nat (Arrow Nat (Arrow Nat Nat))]

nats :: [Exp]
nats = map int2nat [0,1]

vars :: [Exp]
vars = [(Var c) | c <- ['a'..'d']]

alpha_equiv :: [(String, String)] -> Exp -> Exp -> Bool
alpha_equiv _ Z        Z        = True
alpha_equiv c (Succ n) (Succ m) = alpha_equiv c n m

generate :: Int -> [Exp]
generate 0 = nats ++ vars
generate n = gens ++ apps ++ lams ++ recs 
  where
    gens = (generate (n-1))
    apps = [ Ap k l | k <- gens, l <- gens] 
    lams = [ Lambda t k l | k <- vars, l <- gens, t <- types]
    recs = [ Rec e0 v w e1 e | v <- vars, w <- vars, e <- vars, e0 <- gens, e1 <- gens]
