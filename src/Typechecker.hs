module Typechecker (typecheck) where

import Data.List (find)

import Types

typecheck :: Exp -> Either String Typ
typecheck = typecheck' [] 

typecheck' :: Gamma -> Exp -> Either String Typ
-- Basics
typecheck' g Z          = Right Nat
typecheck' g (Succ e)   = case typecheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> Right Nat
  (Right tau) -> Left $ typeError e (show Nat) tau
typecheck' g var@(Var v) = case find (\(trm, tau) -> trm == var) g of
  (Nothing)      -> Left $ "Could not find a type for " ++ show var
  (Just (x,tau)) -> Right tau
typecheck' g (Lambda tau x e) = typecheck' ((x, tau):g) e >>= \sigma -> pure (Arrow tau sigma)
typecheck' g (Ap e1 e2) = case typecheck' g e1 of
  (Left err)           -> Left err
  (Right (Arrow s1 s2))-> case typecheck' g e2 of
    (Left er) -> Left er
    (Right t) -> if t == s1 then Right s2 else Left $ typeError e2 (show s1) t
  (Right tau)          -> Left $ typeError e1 "function type" tau
typecheck' g (Rec e0 x y e1 e)  = case typecheck' g e of
  (Left err)  -> Left err
  (Right Nat) -> case typecheck' g e0 of
    (Left err)  -> Left err
    (Right tau) -> case typecheck' ((x,Nat):(y,tau):g) e1 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then Right tau
                                       else Left $ typeError e1 (show tau) sigma
  (Right tau) -> Left $ typeError e0 (show Nat) tau
-- Booleans
typecheck' g Truth        = Right $ Boolean
typecheck' g Falsehood    = Right $ Boolean
typecheck' g (If t bt bf) = case typecheck' g t of
  (Left err)      -> Left err
  (Right Boolean) -> case typecheck' g bt of
    (Left err) -> Left err
    (Right t)  -> case typecheck' g bf of
      (Left err) -> Left err
      (Right s)  -> if s == t then Right t else Left $ typeError bf (show t) s
  (Right tau)     -> Left $ typeError t (show Boolean) tau
-- Option Types
typecheck' g (Empty tau) = pure $ Option tau
typecheck' g (Full e)    = (typecheck' g e) >>= (pure . Option)
typecheck' g (Which t e1 x e2 e) = case typecheck' g e of
  (Left err)        -> Left err
  (Right (Sum t s)) -> case typecheck' g e1 of
    (Left err)  -> Left err
    (Right tau) -> case typecheck' ((x,t):g) e2 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then Right sigma
                                       else Left $ typeError e2 (show tau) sigma
  (Right tau)       -> Left $ typeError e "sum type" tau 
-- Product Types
typecheck' g Triv          = pure Unit
typecheck' g (Tuple e1 e2) = do t1 <- typecheck' g e1 ; t2 <- typecheck' g e2
                                pure $ Product t1 t2
typecheck' g (Pi_one e)    = case typecheck' g e of
  (Left err)            -> Left err
  (Right (Product t s)) -> pure t
  (Right sigma)         -> Left $ typeError e "product type" sigma
typecheck' g (Pi_two e)    = case typecheck' g e of
  (Left err)            -> Left err
  (Right (Product t s)) -> pure s
  (Right sigma)         -> Left $ typeError e "product type" sigma
-- Sum Type
typecheck' g (InL s t e) = case typecheck' g e of
  (Left err)    -> Left err
  (Right sigma) -> if sigma == s then pure sigma else Left $ typeError e (show s) sigma
typecheck' g (InR s t e) = case typecheck' g e of
  (Left err)  -> Left err
  (Right tau) -> if tau == t then pure tau else Left $ typeError e (show t) tau
typecheck' g (Abort t e) = case typecheck' g e of 
  (Left err)   -> Left err
  (Right Void) -> pure t
  (Right tau)  -> Left $ typeError e (show Void) tau
typecheck' g (Case e x e1 y e2) = case typecheck' g e of
  (Left err)        -> Left err
  (Right (Sum t s)) -> case typecheck' ((x,t):g) e1 of
    (Left err)  -> Left err 
    (Right tau) -> case typecheck' ((y,s):g) e2 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then pure tau else Left $ typeError e2 (show tau) sigma
  (Right tau)       -> Left $ typeError e "sum type" tau

typeError :: Exp -> String -> Typ -> String
typeError e s t = "Type Error!\nExpression: " ++ show e ++
                  "\nExpected: " ++ s ++ ", found " ++ show t
