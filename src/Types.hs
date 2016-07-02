module Types where

import Data.List (intercalate)

-- Context for typing derivations
type Gamma = [(Exp, Typ)]

-- Context for evaluation
type Context = [(String, Exp)]

-- Haskell type of GödelT types
data Typ = Void
         | Unit
         | Nat
         | Boolean
         | Option  Typ
         | Arrow   Typ Typ
         | Product Typ Typ
         | Sum     Typ Typ
         deriving (Eq, Ord, Show)

-- Haskell type of GödelT expressions
data Exp = Placeholder String        -- Placeholder
         | Z                         -- Zero
         | Succ Exp                  -- Successor
         | Var Char                  -- Variables
         | Lambda Typ Exp Exp        -- Lambda 
         | Rec Exp Exp Exp Exp Exp   -- Recursor
         | Ap Exp Exp                -- Application
         -- Product Types
         | Triv                      -- Empty  tuple
         | Tuple  Exp Exp            -- Binary tuple
         | Pi_one Exp                -- Projection to first  element
         | Pi_two Exp                -- Projection to second element
         -- Sum Type
         | Abort Typ Exp             -- Diverging computation
         | Case  Exp Exp Exp Exp Exp -- Case analysis on sums
         | InL   Typ Typ Exp         -- InLeft
         | InR   Typ Typ Exp         -- InRight
         -- Option Types
         | Empty Typ                 -- No  Value
         | Full Exp                  -- Yes Value
         | Which Typ Exp Exp Exp Exp -- Case analysis on options
         -- Booleans
         | Truth                     -- Truth constant
         | Falsehood                 -- False constant
         | If Exp Exp Exp            -- If/Then/Else construct
         deriving (Eq, Ord, Show)

-- Inputs expected on REPL
data Input = Run
           | Quit
           | Help
           | Context
           | Clear
           | Let String Exp
           | Expr       Exp
           | Check      Exp
           | NoParse
           deriving Eq

------------------------------------------------------

{--

-- Beautiful Types
instance Show Typ where
  show Nat           = "ℕ"
  show Void          = "𝟘"
  show Unit          = "𝟙"
  show Boolean       = "𝟚"
  show (Option  t  ) = "(" ++ show t ++ ")?"
  show (Arrow   t s) = "(" ++ show t ++ " → " ++ show s ++ ")"
  show (Product t s) = "(" ++ show t ++ " ⨯ " ++ show s ++ ")"
  show (Sum     t s) = "(" ++ show t ++ " + " ++ show s ++ ")"

-- Beautiful Expressions
instance Show Exp where
  show (Placeholder str)   = "_" ++ str ++ "_" 
  -- Basics
  show (Z)                 = "Z"
  show (Succ e)            = "S(" ++ show e ++ ")"
  show (Var c)             = [c]
  show (Lambda t v b)      = "λ(" ++ show v ++ " : " ++ show t ++ ")." ++ show b
  show (Rec e0 x y e1 e)   = "rec " ++ show e ++ " { Z ~> " ++ show e0 ++ " | S(" ++
                             show x ++ ") with " ++ show y ++ " ~> " ++ show e1 ++ " }"   
  show (Ap e1 e2)          = '(':show e1 ++ "[" ++ show e2 ++ "])"
  -- Booleans
  show Truth               = "true"
  show Falsehood           = "false"
  show (If t bt bf)        = "if " ++ show t ++ " then " ++ show bt ++ " else " ++ show bf
  -- Option Types
  show (Empty tau)         = "({} : " ++ show tau ++ ")"
  show (Full e)            = "{" ++ show e ++ "}"
  show (Which t e0 x e1 e) = "which " ++ show e ++ "{({} : " ++ show t ++ ") ~> " ++ show e0
                             ++ " | full(" ++ show x ++ ") ~> " ++ show e1 ++ " }"
  -- Product Types
  show Triv                = "<>"
  show (Tuple e1 e2)       = "<" ++ show e1 ++ ", " ++ show e2 ++ ">"
  show (Pi_one e)          = "π1(" ++ show e ++ ")"
  show (Pi_two e)          = "π2(" ++ show e ++ ")"
  -- Sum Types
  show (Abort t e)         = "abort(" ++ show e ++ ")"
  show (InL t1 t2 e)       = "inL("   ++ show e ++ ") : " ++ show (Sum t1 t2)
  show (InR t1 t2 e)       = "inR("   ++ show e ++ ") : " ++ show (Sum t1 t2)
  show (Case e x e1 y e2)  = "case " ++ show e ++ " { inL(" ++ show x ++ ") ~> " ++ show e1 
                              ++ " | inR(" ++ show y ++ ") ~> " ++ show e2 ++ " }"

--}
