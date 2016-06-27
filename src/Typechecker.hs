module Typechecker (typecheck) where

import Data.List (find)

import Types

typecheck :: Exp -> Either String Typ
typecheck = typeCheck' [] 

typeCheck' :: Gamma -> Exp -> Either String Typ
typeCheck' g Z          = Right Nat
typeCheck' g (Succ e)   = case typeCheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> Right Nat
  (Right foo) -> Left $ typeError e Nat foo
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
    (Right t) -> if t == s1 then Right s2 else Left $ typeError e2 s1 t
  (Right (Nat))        -> Left $ "Type Error!\nExpression: " ++ show e1 ++ "Expected function type, found nat"
typeCheck' g (Rec e0 x y e1 e)  = case typeCheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> case typeCheck' g e0 of
    (Left err)  -> Left err
    (Right tau) -> case typeCheck' ((x,Nat):(y,tau):g) e1 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then Right tau
                                       else Left $ typeError e1 tau sigma
  (Right tau) -> Left $ typeError e0 Nat tau
typeCheck' g Truth        = Right $ Boolean
typeCheck' g Falsehood    = Right $ Boolean
typeCheck' g (If t bt bf) = case typeCheck' g t of
  (Left err)      -> Left err
  (Right Boolean) -> case typeCheck' g bt of
    (Left err) -> Left err
    (Right t)  -> case typeCheck' g bf of
      (Left err) -> Left err
      (Right s)  -> if s == t then Right t else Left $ typeError bf t s
  (Right tau)     -> Left $ typeError t Boolean tau


typeError :: Exp -> Typ -> Typ -> String
typeError e t1 t2 = "Type Error!\nExpression: " ++ show e ++
                    "\nExpected: " ++ show t1 ++ ", found " ++ show t2 
