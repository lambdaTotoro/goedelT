{-# LANGUAGE OverloadedStrings #-}

module Parser
( parseTyp, parseExp, parseInput, parseFile )
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
              <|> ((string ":run")     >> pure Run)
              <|> letParser
              <|> typecheckParser
              <|> evalParser
    
    loadParser :: Parser Input
    loadParser = do string ":load "
                    filepth <- manyTill letter_ascii endOfLine
                    pure $ Load filepth

    letParser :: Parser Input
    letParser = do string ":let "
                   name <- manyTill letter_ascii space
                   string "= "
                   exp <- expParser
                   pure $ Let name exp

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
typParser = pNat <|> pVoid <|> pUnit <|> pBool <|> pOption <|> pSum <|> pProduct <|> pArrow
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
expParser = pBools <|> pIf  <|> pEmpty <|> pFull <|> pWhich <|> pTuple <|> pProject <|> 
            pVoid <|> pSum <|> pCase <|> pAp  <|> pLambda <|> pRec <|> pZ <|> pSucc 
            <|> pVar <|> pPlaceholder
  where
    pPlaceholder :: Parser Exp
    pPlaceholder = do char '_' ; g <- many1 letter_ascii ; char '_'
                      pure $ Placeholder g
                      <?> "Placeholder parser"

    pZ :: Parser Exp
    pZ = (string "0" <|> string "Z") >> pure Z
         <?> "Zero parser"

    pSucc :: Parser Exp
    pSucc = do string "S(" ; e <- expParser ; char ')' 
               pure $ Succ e
            <?> "Successor parser"

    pVar :: Parser Exp
    pVar = do j <- (many1 letter_ascii) 
              pure (Var j)
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

    -- Boolean
    
    pBools :: Parser Exp
    pBools = (string "true" >> pure Truth)      <|> (string "tt" >> pure Truth) <|>
             (string "false" >> pure Falsehood) <|> (string "ff" >> pure Falsehood)

    pIf :: Parser Exp
    pIf = do string "if "    ; t  <- expParser
             string " then " ; bt <- expParser
             string " else " ; bf <- expParser
             pure $ If t bt bf 

    -- Option Types
    
    pEmpty :: Parser Exp
    pEmpty = do string "({} : " ; tau <- typParser ; char ')' ; pure $ Empty tau

    pFull :: Parser Exp
    pFull = do string "{ " ; e <- expParser ; string " }" ; pure $ Full e

    pWhich :: Parser Exp
    pWhich = do string "which " ; e <- expParser; string "{({} : "
                tau <- typParser ; ") ~> " ; e0 <- expParser
                string " | full(" ; x <- pVar 
                string ") ~> "  ; e1 <- expParser ; string " }"
                pure $ Which tau e0 x e1 e

    -- Product Types
    
    pTuple :: Parser Exp
    pTuple = (string "<>" >> pure Triv) <|>
             do char '<' ; e1 <- expParser ; string ", "
                e2 <- expParser ; char '>'
                pure $ Tuple e1 e2

    pProject :: Parser Exp
    pProject = pi1 <|> pi2 
      where
        pi1 = do ((string "pi1(") <|> (string "π1("))
                 tpl <- expParser ; char ')'
                 pure (Pi_one tpl)
        pi2 = do ((string "pi2(") <|> (string "π2("))
                 tpl <- expParser ; char ')'
                 pure (Pi_two tpl)

    -- Sum Types
 
    pVoid :: Parser Exp
    pVoid = do string "(abort(" ; e <- expParser ; string ") : "
               tau <- typParser; char ')' ; pure (Abort tau e)

    pSum :: Parser Exp
    pSum = sum1 <|> sum2
      where
        sum1 = do string "inL(" ; e <- expParser ; string ") : ("
                  tau   <- typParser ; string " + "
                  sigma <- typParser ; char ')'
                  pure $ InL tau sigma e

        sum2 = do string "inR(" ; e <- expParser ; string ") : ("
                  tau   <- typParser ; string " + "
                  sigma <- typParser ; char ')'
                  pure $ InL tau sigma e

    pCase :: Parser Exp
    pCase = do string "case " ; e <- expParser ; string " { inL("
               x <- pVar ; string ") ~> " ; e1 <- expParser
               string " | inR(" ; y <- pVar ; string ") ~> "
               e2 <- expParser ; string " } " ; pure $ Case e x e1 y e2

------------------
-- File Parsing --
------------------

parseFile :: String -> Either String Context
parseFile = undefined 

fileParser :: Parser Context
fileParser = many1 defParser
  where
    defParser :: Parser (String, Exp)
    defParser = undefined
    
