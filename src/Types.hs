module Types where

import Data.List (find)

type Gamma = [(Exp, Typ)]

data Typ = Nat
         | Arrow Typ Typ
         deriving (Eq, Ord)

data Exp = Z                       -- Zero
         | Succ Exp                -- Successor
         | Var Char                -- Variables
         | Lambda Typ Exp Exp      -- Lambda 
         | Rec Exp Exp Exp Exp Exp -- Recursor
         | Ap Exp Exp              -- Application
         deriving (Eq, Ord)


data Input = Quit
           | Help
           | Context
           | Clear
           | Let String Exp
           | Expr Exp
           | NoParse
           deriving Eq

------------------------------------------------------

instance Show Typ where
  show Nat           = "ℕ"
  show (Arrow t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")" 


instance Show Exp where
  show (Z)               = "0"
  show (Succ e)          = "S(" ++ show e ++ ")"
  show (Var c)           = [c]
  show (Lambda t v b)    = "λ(" ++ show v ++ " : " ++ show t ++ ")." ++ show b
  show (Rec e0 x y e1 e) = "rec " ++ show e ++ " { Z ~> " ++ show e0 ++ " | S(" ++
                           show x ++ ") with " ++ show y ++ " ~> " ++ show e1 ++ " }"   
  show (Ap e1 e2)        = '(':show e1 ++ "[" ++ show e2 ++ "])"
--
------------------------------------------------------

typeCheck :: Gamma -> Exp -> Either String Typ
typeCheck g Z          = Right Nat
typeCheck g (Succ e)   = case typeCheck g e of
  (Left err)  -> Left err
  (Right Nat) -> Right Nat
  (Right foo) -> Left $ typeError e Nat foo
typeCheck g var@(Var v) = case find (\(trm, tau) -> trm == var) g of
  (Nothing)      -> Left $ "Could not find a type for " ++ show var
  (Just (x,tau)) -> Right tau
typeCheck g (Lambda tau x e) = case typeCheck ((x, tau):g) e of
  (Left err) -> Left err
  (Right t)  -> Right (Arrow tau t)
typeCheck g (Ap e1 e2) = case typeCheck g e1 of
  (Left err)           -> Left err
  (Right (Arrow s1 s2))-> case typeCheck g e2 of
    (Left er) -> Left er
    (Right t) -> if t == s1 then Right s2 else Left $ typeError e2 s1 t
  (Right (Nat))        -> Left $ "Type Error!\nExpression: " ++ show e1 ++ "Expected function type, found nat"
typeCheck g (Rec e0 x y e1 e)  = case typeCheck g e of
  (Left err)  -> Left err
  (Right Nat) -> case typeCheck g e0 of
    (Left err)  -> Left err
    (Right tau) -> case typeCheck ((x,Nat):(y,tau):g) e1 of
      (Left err)    -> Left err
      (Right sigma) -> if tau == sigma then Right tau
                                       else Left $ typeError e1 tau sigma
  (Right tau) -> Left $ typeError e0 Nat tau

check :: Exp -> IO ()
check e = case typeCheck [] e of { (Left err) -> print err ; (Right tau) -> print tau}

typeError :: Exp -> Typ -> Typ -> String
typeError e t1 t2 = "Type Error!\nExpression: " ++ show e ++
                    "\nExpected: " ++ show t1 ++ ", found " ++ show t2 
