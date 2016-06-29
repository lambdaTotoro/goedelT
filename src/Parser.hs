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
typParser = pNat <|> pArrow
  where
    pNat :: Parser Typ
    pNat = (string "ℕ"   >> pure Nat) <|> 
           (string "nat" >> pure Nat) <|> 
           (string "Nat" >> pure Nat)

    pVoid :: Parser Typ
    pVoid = (string "𝟘"    >> pure Void) <|>
            (string "void" >> pure Void) <|>
            (string "Void" >> pure Void)

    pUnit :: Parser Typ
    pUnit = (string "𝟙"    >> pure Unit) <|>
            (string "unit" >> pure Unit) <|>
            (string "Unit" >> pure Unit)

    pBool :: Parser Typ
    pBool = (string "𝟚"       >> pure Boolean) <|>
            (string "Bool"    >> pure Boolean) <|> (string "bool"    >> pure Boolean) <|>
            (string "Boolean" >> pure Boolean) <|> (string "boolean" >> pure Boolean)

    pOption :: Parser Typ
    pOption = do string "(" ; tau <- typParser ; string ")?" ; pure (Option tau)
 
    pArrow :: Parser Typ
    pArrow = do char '(' ; tau <- typParser 
                ((string " -> ") <|> (string " → "))
                sigma <- typParser ; char ')'
                pure $ Arrow tau sigma

    pProduct :: Parser Typ
    pProduct = do char '(' ; tau <- typParser
                  ((string " x ") <|> (string " ⨯ "))
                  sigma <- typParser ; char ')'
                  pure $ Product tau sigma
                  
    pSum :: Parser Typ
    pSum = do char '(' ; tau <- typParser ; string " + "
              sigma <- typParser ; char ')' ; pure $ Sum tau sigma

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
