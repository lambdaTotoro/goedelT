module Types where

import Data.List (intercalate)

type Gamma = [(Exp, Typ)]

data Typ = Nat
         | Boolean
         | Arrow   Typ Typ
         | Product [Typ]
         | Sum     [Typ]
         | Option  Typ
         deriving (Eq, Ord)

data Exp = Z                       -- Zero
         | Succ Exp                -- Successor
         | Var Char                -- Variables
         | Lambda Typ Exp Exp      -- Lambda 
         | Rec Exp Exp Exp Exp Exp -- Recursor
         | Ap Exp Exp              -- Application
         -- Product Types
         | Tuple [(Exp, Exp)]
         | Projection Exp Exp
         -- Sum Types
         -- Option Types
         | Empty
         | Full Exp
         | Which Typ Exp Exp Exp Exp
         -- Booleans
         | Truth
         | Falsehood
         | If Exp Exp Exp
         deriving (Eq, Ord)


data Input = Quit
           | Help
           | Context
           | Clear
           | Let String Exp
           | Expr Exp
           | Check Exp
           | NoParse
           deriving Eq

------------------------------------------------------

instance Show Typ where
  show Nat           = "ℕ"
  show Boolean       = "𝟚"
  show (Arrow t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")"
  show (Product [])  = "𝟙"
  show (Product ts)  = "(" ++ intercalate "⨯" (map show ts) ++ ")"
  show (Sum     [])  = "𝟘"
  show (Sum     ts)  = "(" ++ intercalate "+" (map show ts) ++ ")"
  show (Option tau)  = show tau ++ "?"

instance Show Exp where
  show (Z)               = "0"
  show (Succ e)          = "S(" ++ show e ++ ")"
  show (Var c)           = [c]
  show (Lambda t v b)    = "λ(" ++ show v ++ " : " ++ show t ++ ")." ++ show b
  show (Rec e0 x y e1 e) = "rec " ++ show e ++ " { Z ~> " ++ show e0 ++ " | S(" ++
                           show x ++ ") with " ++ show y ++ " ~> " ++ show e1 ++ " }"   
  show (Ap e1 e2)        = '(':show e1 ++ "[" ++ show e2 ++ "])"

