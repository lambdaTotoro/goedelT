{-# LANGUAGE OverloadedStrings #-}

module Parser
( parseTyp, parseExp )
where

import Control.Applicative ((<|>))
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8

import Types

parseTyp :: String -> Either String Typ
parseTyp = (parseOnly typParser) . pack

parseExp :: String -> Either String Exp
parseExp = (parseOnly expParser) . pack

--------

typParser :: Parser Typ
typParser = pNat <|> pArr
  where
    pNat :: Parser Typ
    pNat = (string "ℕ"   >> pure Nat) <|> 
           (string "nat" >> pure Nat) <|> 
           (string "Nat" >> pure Nat)

    pArr :: Parser Typ
    pArr = do char '(' ; t1 <- typParser 
              ((string " -> ") <|> (string " → "))
              t2 <- typParser ; char ')'
              pure $ Arrow t1 t2

expParser :: Parser Exp
expParser = pAp <|> pLambda <|> pRec <|> pZ <|> pSucc <|> pVar
  where
    pZ :: Parser Exp
    pZ = (string "0" <|> string "Z") >> pure Z
         <?> "Zero parser"

    pSucc :: Parser Exp
    pSucc = do string "S(" ; e <- expParser ; char ')' 
               pure $ Succ e
            <?> "Successor parser"

    pVar :: Parser Exp
    pVar = do j <- letter_ascii ; pure (Var j)
           <?> "Variable parser"

    pLambda :: Parser Exp
    pLambda = do (string "λ(" <|> string "lambda(")
                 v <- pVar ; string " : " ; t <- typParser
                 string ")." ; b <- expParser
                 pure $ Lambda t v b
              <?> "Lambda parser"

    pRec :: Parser Exp
    pRec = do string "rec " ; e <- pVar ; string " { Z ~> "
              e0 <- expParser ; string " | S(" ; x <- pVar
              string ") with " ; y <- pVar ; string " ~> " 
              e1 <- expParser ; string " }" 
              pure $ Rec e0 x y e1 e
           <?> "Recursor parser"

    pAp :: Parser Exp
    pAp = do char '(' ; t1 <- expParser ; char '['
             t2 <- expParser ; char ']' ; char ')'
             pure $ Ap t1 t2
          <?> "Application parser"

