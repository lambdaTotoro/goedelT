module Typechecker (typecheck) where

import Data.List (find)

import Types

typecheck :: Exp -> Either String Typ
typecheck = typeCheck' [] 

typeCheck' :: Gamma -> Exp -> Either String Typ
-- Basics
typeCheck' g Z          = Right Nat
typeCheck' g (Succ e)   = case typeCheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> Right Nat
  (Right tau) -> Left $ typeError e (show Nat) tau
typeCheck' g var@(Var v) = case find (\(trm, tau) -> trm == var) g of
  (Nothing)      -> Left $ "Could not find a type for " ++ show var
  (Just (x,tau)) -> Right tau
typeCheck' g (Lambda tau x e) = case typeCheck' ((x, tau):g) e of
  (Left err) -> Left err
  (Right t)  -> Right (Arrow tau t)
typeCheck' g (Ap e1 e2) = case typeCheck' g e1 of
  (Left err)           -> Left err
  (Right (Arrow s1 s2))-> case typeCheck' g e2 of
    (Left er) -> Left er
    (Right t) -> if t == s1 then Right s2 else Left $ typeError e2 (show s1) t
  (Right tau)          -> Left $ typeError e1 "function type" tau
typeCheck' g (Rec e0 x y e1 e)  = case typeCheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> case typeCheck' g e0 of
    (Left err)  -> Left err
    (Right tau) -> case typeCheck' ((x,Nat):(y,tau):g) e1 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then Right tau
                                       else Left $ typeError e1 (show tau) sigma
  (Right tau) -> Left $ typeError e0 (show Nat) tau
-- Booleans
typeCheck' g Truth        = Right $ Boolean
typeCheck' g Falsehood    = Right $ Boolean
typeCheck' g (If t bt bf) = case typeCheck' g t of
  (Left err)      -> Left err
  (Right Boolean) -> case typeCheck' g bt of
    (Left err) -> Left err
    (Right t)  -> case typeCheck' g bf of
      (Left err) -> Left err
      (Right s)  -> if s == t then Right t else Left $ typeError bf (show t) s
  (Right tau)     -> Left $ typeError t (show Boolean) tau
-- Option Types
typeCheck' g Empty    = undefined
typeCheck' g (Full e) = case typeCheck' g e of
  (Left err)  -> Left err
  (Right tau) -> Right $ Option tau
typeCheck' g (Which t e1 x e2 e) = case typeCheck' g e of
  (Left err)        -> Left err
  (Right (Sum t s)) -> undefined
  (Right tau)       -> Left $ typeError e "sum type" tau 
-- Product Types
-- Sum Type

typeError :: Exp -> String -> Typ -> String
typeError e s t = "Type Error!\nExpression: " ++ show e ++
                  "\nExpected: " ++ s ++ ", found " ++ show t
