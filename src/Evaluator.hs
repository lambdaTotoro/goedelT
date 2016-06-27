module Evaluator where

import Types

isValue :: Exp -> Bool
isValue (Succ e)              = isValue e
isValue (Lambda t v e)        = isValue e
isValue (Ap (Lambda _ _ _) x) = False
isValue (If t _ _)            = isValue t
isValue _                     = True

replace :: Exp -> Exp -> Exp -> Exp
replace _ _ (Z)             = Z
replace e v (Succ n)        = Succ (replace e v n)
replace e v var@(Var x)     | var == v  = e
                            | otherwise = var
replace e v (Ap e1 e2)      = Ap (replace e v e1) (replace e v e2)
replace e v (Lambda t x b)  = Lambda t x (replace e v b)
replace e v (Rec n x y o r) = Rec (replace e v n) x y (replace e v o) (replace e v r)
replace e v (If t bt bf)    = If (replace e v t) (replace e v bt) (replace e v bf)


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
eval (If Truth bt _ )    = bt
eval (If Falsehood _ bf) = bf
eval (If v bt bf)        = If (eval v) bt bf
eval x                   = x 
