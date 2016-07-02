module Evaluator where

import Types

-- Evaluates a term
eval :: Exp -> Exp
eval (Succ e)        = Succ (eval e)
eval ap@(Ap e1 e2)  
  | not (val e1)     = eval $ (Ap (eval e1) e2)
  | not (val e2)     = eval $ (Ap e1 (eval e2))
  | otherwise        = case e1 of 
    (Lambda tau x e) -> eval $ replace e2 x e
    _                -> ap
eval (Rec e0 x y e1 q)
  | not (val q)      = eval (Rec e0 x y e1 (eval q))
  | otherwise        = case q of
    (Z)      -> eval e0
    (Succ e) -> eval $ replace (Rec e0 x y e1 e) y (replace e x e1)
-- Booleans
eval (If Truth bt _ )    = bt
eval (If Falsehood _ bf) = bf
eval (If v bt bf)        = eval $ If (eval v) bt bf
-- Product Type
eval (Tuple e1 e2)
  | not (val e1) = eval $ Tuple (eval e1) e2
  | not (val e2) = eval $ Tuple e1 (eval e2)
  | otherwise    = Tuple e1 e2
eval (Pi_one (Tuple e1 e2))
  | not (val e1) = eval $ Pi_one (Tuple (eval e1) e2)
  | not (val e2) = eval $ Pi_one (Tuple e1 (eval e2))  
  | otherwise    = e1
eval (Pi_two (Tuple e1 e2))
  | not (val e1) = eval $ Pi_two (Tuple (eval e1) e2)
  | not (val e2) = eval $ Pi_two (Tuple e1 (eval e2))
  | otherwise    = e2  
-- Sum Type
eval (Abort t e)
  | not (val e) = eval $ Abort t (eval e)
eval l@(InL t1 t2 e) 
  | not (val e) = eval $ InL t1 t2 (eval e)
  | otherwise   = l
eval r@(InR t1 t2 e)
  | not (val e) = eval $ InR t1 t2 (eval e)
  | otherwise   = r
eval (Case e x e1 y e2)
  | not (val e) = eval $ Case (eval e) x e1 y e2
  | otherwise   = case e of
    (InL _ _ q) -> eval $ replace q x e1
    (InR _ _ q) -> eval $ replace q y e2
-- Option Types
eval (Full e)
  | not (val e) = eval $ Full (eval e)
  | otherwise   = Full e
eval (Which t e0 x e1 e)
  | not (val e) = eval $ Which t e0 x e1 (eval e)
  | otherwise   = case e of
    (Empty _) -> eval e0
    (Full q)  -> eval (replace q x e1)
-- Otherwise
eval x          = x 

-- Takes an expresion and returns, wether or not
-- that expression is already a fully formed value.
val :: Exp -> Bool
-- Basics
val Z              = True
val (Succ e)       = val e
val (Lambda _ _ _) = True
-- Booleans
val Truth          = True
val Falsehood      = True
-- Option Types
val (Empty _)      = True
val (Full e)       = val e
-- Product Types
val Triv           = True
val (Tuple e1 e2)  = val e1 && val e2
-- Sum Types
val (InL _ _ e)    = val e
val (InR _ _ e)    = val e
-- Otherwise
val _              = False

-- Replaces occurences of variables (second expression) with the
-- first expression in the third expression. 
-- In other words: return 4 = replace 2 by 1 in 3
replace :: Exp -> Exp -> Exp -> Exp
-- Basics
replace e v (Z)         = if v == Z then e else Z
replace e v k@(Succ n)  = if v == k then e else Succ (replace e v n)
replace e v k@(Var c)   = if v == k then e else k
replace e v k@(Ap e1 e2)
  | v == k    = e    
  | otherwise = Ap (replace e v e1) (replace e v e2)
replace e v k@(Lambda t x b)
  | v == k    = e
  | v == x    = Lambda t x b
  | otherwise = Lambda t (replace e v x) (replace e v b)
replace e v k@(Rec e0 x y e1 q)   
  | v == k           = e
  | v == x || v == y = Rec (replace e v e0) x y e1 q
  | otherwise        = Rec (replace e v e0) x y (replace e v e1) (replace e v q)
-- Product Types
replace e v k@(Triv)
  | v == k    = e
  | otherwise = Triv
replace e v k@(Tuple e1 e2)
  | v == k    = e
  | otherwise = Tuple (replace e v e1) (replace e v e2) 
replace e v k@(Pi_one q) 
  | v == k    = e
  | otherwise = Pi_one (replace e v q) 
replace e v k@(Pi_two q)
  | v == k    = e
  | otherwise = Pi_two (replace e v q)
-- Sum Types
replace e v k@(Abort t q)
  | v == k    = e
  | otherwise = Abort t (replace e v q)
replace e v k@(InL t1 t2 q)
  | v == k    = e
  | otherwise = InL t1 t2 (replace e v q)
replace e v k@(InR t1 t2 q)
  | v == k    = e
  | otherwise = InR t1 t2 (replace e v q)
replace e v k@(Case q x e1 y e2)  
  | v == k           = e
  | v == x && v == y = Case q x e1 y e2
  | v == x           = Case (replace e v q) x e1 y (replace e v e2)
  | v == y           = Case (replace e v q) x (replace e v e1) y e2
  | otherwise        = Case (replace e v q) x (replace e v e1) y (replace e v e2)
-- Option Types
replace e v k@(Empty t)
  | v == k    = e
  | otherwise = Empty t
replace e v k@(Full q)
  | v == k    = e
  | otherwise = Full (replace e v q)
replace e v k@(Which t x e1 y e2)
  | v == k           = e
  | v == x && v == y = Which t x e1 y e2
  | v == x           = Which t x e1 y (replace e v e2)
  | v == y           = Which t x (replace e v e1) y e2
  | otherwise        = Which t (replace e v x) (replace e v e1) (replace e v y) (replace e v e2)
-- Booleans
replace e v k@(Truth)           = if v == k then e else Truth
replace e v k@(Falsehood)       = if v == k then e else Falsehood
replace e v k@(If t bt bf) 
  | v == k    = e
  | otherwise = If (replace e v t) (replace e v bt) (replace e v bf)
