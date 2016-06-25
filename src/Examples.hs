module Examples where

import Types

int2nat :: Int -> Exp
int2nat 0 = Z
int2nat n = Succ (int2nat (n-1))

nat2int :: Exp -> Int
nat2int Z        = 0
nat2int (Succ e) = 1 + (nat2int e)

plus :: Int -> Int -> Exp
plus m n = (Ap (Ap add m') n')
  where 
    m' = int2nat m
    n' = int2nat n

const_nat :: Int -> Exp
const_nat n = Lambda Nat (Var 'a') (int2nat n)

-- add : nat -> nat -> nat
add :: Exp
add = Lambda Nat (Var 'a') (Lambda Nat (Var 'b')
        ( Rec (Var 'a') (Var 'c') (Var 'd') (Succ (Var 'd')) (Var 'b')))

-- double : nat -> nat
double :: Exp
double = Lambda Nat (Var 'a') (Rec Z (Var 'b') (Var 'c') (Succ (Succ (Var 'c'))) (Var 'a'))
