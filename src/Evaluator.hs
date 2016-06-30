module Evaluator where

import Types

isValue :: Exp -> Bool
isValue (Succ e)              = isValue e
isValue (Lambda t v e)        = isValue e
isValue (Ap (Lambda _ _ _) x) = False
isValue (If t _ _)            = isValue t
isValue (Tuple e1 e2)         = isValue e1 && isValue e2
isValue (InL _ _ e)           = isValue e
isValue (InR _ _ e)           = isValue e
isValue (Full e)              = isValue e
isValue _                     = True

replace :: Exp -> Exp -> Exp -> Exp
-- Basics
replace _ _ (Z)                 = Z
replace e v (Succ n)            = Succ (replace e v n)
replace e v var@(Var x)         | var == v  = e
                                | otherwise = var
replace e v (Ap e1 e2)          = Ap (replace e v e1) (replace e v e2)
replace e v (Lambda t x b)      = Lambda t x (replace e v b)
replace e v (Rec n x y o r)     = Rec (replace e v n) x y (replace e v o) (replace e v r)
-- Product Types
replace e v (Triv)              = Triv
replace e v (Tuple e1 e2)       = Tuple (replace e v e1) (replace e v e2)
replace e v (Pi_one q)          = Pi_one (replace e v q)
replace e v (Pi_two q)          = Pi_two (replace e v q)
-- Sum Types
replace e v (Abort t q)         = Abort t (replace e v q)
replace e v (InL t1 t2 q)       = InL t1 t2 (replace e v q)
replace e v (InR t1 t2 q)       = InR t1 t2 (replace e v q)
replace e v (Case q x e1 y e2)  = Case (replace e v q) x (replace e v e1) y (replace e v e2)
-- Option Types
replace e v (Empty t)           = Empty t
replace e v (Full q)            = Full (replace e v q)
replace e v (Which t x e1 y e2) = Which t x (replace e v e1) y (replace e v e2)
-- Booleans
replace e v Truth               = Truth
replace e v Falsehood           = Falsehood
replace e v (If t bt bf)        = If (replace e v t) (replace e v bt) (replace e v bf)

eval :: Exp -> Exp
eval (Succ e)          = Succ (eval e)
eval ap@(Ap e1 e2)  
  | not (isValue e1)   = eval (Ap (eval e1) e2)
  | not (isValue e2)   = eval (Ap e1 (eval e2))
  | otherwise          = case e1 of 
    (Lambda tau x e) -> eval (replace e2 x e)
    _                -> ap
eval (Rec e0 x y e1 q)
  | not (isValue q) = eval (Rec e0 x y e1 (eval q))
  | otherwise       = case q of
    (Z)      -> eval e0
    (Succ e) -> eval $ replace (Rec e0 x y e1 e) y (replace e x e1)
-- Booleans
eval (If Truth bt _ )    = bt
eval (If Falsehood _ bf) = bf
eval (If v bt bf)        = If (eval v) bt bf
-- Product Type
eval (Tuple e1 e2)
  | not (isValue e1) = eval $ Tuple (eval e1) e2
  | not (isValue e2) = eval $ Tuple e1 (eval e2)
  | otherwise        = Tuple e1 e2
eval (Pi_one (Tuple e1 e2))
  | not (isValue e1) = eval $ Pi_one (Tuple (eval e1) e2)
  | not (isValue e2) = eval $ Pi_one (Tuple e1 (eval e2))  
  | otherwise        = e1
eval (Pi_two (Tuple e1 e2))
  | not (isValue e1) = eval $ Pi_two (Tuple (eval e1) e2)
  | not (isValue e2) = eval $ Pi_two (Tuple e1 (eval e2))
  | otherwise        = e2  
-- Sum Type
eval (Abort t e)
  | not (isValue e) = eval $ Abort t (eval e)
eval l@(InL t1 t2 e) 
  | not (isValue e) = eval $ InL t1 t2 (eval e)
  | otherwise       = l
eval r@(InR t1 t2 e)
  | not (isValue e) = eval $ InR t1 t2 (eval e)
  | otherwise       = r
eval (Case e x e1 y e2)
  | not (isValue e) = eval $ Case (eval e) x e1 y e2
  | otherwise       = case e of
    (InL _ _ q) -> eval $ replace q x e1
    (InR _ _ q) -> eval $ replace q y e2
-- Option Types
eval (Full e)
  | isValue e        = Full e
  | otherwise        = eval $ Full (eval e)
eval (Which t e0 x e1 e)
  | not (isValue e)  = eval $ Which t e0 x e1 (eval e)
  | otherwise        = case e of
    (Empty _)  -> eval e0
    (Full q) -> eval $ replace q x e1
-- Otherwise
eval x               = x 
