{-# LANGUAGE OverloadedStrings #-}

module Parser
( parseTyp, parseExp, parseInput )
where

import Control.Applicative ((<|>))
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8

import Types

parseInput :: String -> Input
parseInput str = case ((parseOnly inputParser) . pack) str of
  (Right i) -> i
  (Left  e) -> NoParse
  where
    inputParser :: Parser Input
    inputParser = ((string ":quit")    >> pure Quit)
              <|> ((string ":context") >> pure Context)
              <|> ((string ":clear")   >> pure Clear)
              <|> ((string ":help")    >> pure Help)
              <|> letParser
              <|> typecheckParser
              <|> evalParser

    letParser :: Parser Input
    letParser = do string ":let "
                   name <- anyChar
                   string " name "
                   exp <- expParser
                   pure $ Let [name] exp

    evalParser :: Parser Input
    evalParser = do string ":eval "
                    exp <- expParser
                    pure $ Expr exp

    typecheckParser :: Parser Input
    typecheckParser = do string ":typecheck "
                         exp <- expParser
                         pure $ Check exp

parseTyp :: String -> Either String Typ
parseTyp = (parseOnly typParser) . pack

parseExp :: String -> Either String Exp
parseExp = (parseOnly expParser) . pack

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

    pBool :: Parser Exp
    pBool = (string "true"  >> pure Truth) 
        <|> (string "false" >> pure Falsehood)
 
    pIf :: Parser Exp
    pIf = do string "if "    ; t  <- expParser
             string " then " ; bt <- expParser
             string " else " ; bf <- expParser
             pure $ If t bt bf
