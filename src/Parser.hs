module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Lexer
import Types

abstraction = do { reservedOp "\\"
                 ; v <- identifier
                 ; reservedOp "."
                 ; t <- expr
                 ; return (SAbs v t)
                 }

variable = do { v <- identifier
              ; return (SId v)
              }

ifthenelse = do { reserved "if"
                ; cond <- expr
                ; reserved "then"
                ; e1 <- expr
                ; reserved "else"
                ; e2 <- expr
                ; return (IfThenElse cond e1 e2)
                }
             
constTrue = do { reserved "true"
               ; return (Boolean True) 
               }
            
constFalse = do { reserved "false"
                ; return (Boolean False)
                }

iszero = do { reserved "iszero"
            ; return IsZero
            }

numeral = do { v <- natural
             ; return (Num v)
             }

nsucc = do { reserved "succ"
           ; return Succ
           }

letin = do { reserved "let"
           ; v <- identifier
           ; reservedOp "="
           ; e1 <- expr
           ; reserved "in"
           ; e2 <- expr
           ; return (LetIn v e1 e2)
           }

  
term = parens expr
       <|> letin
       <|> constFalse
       <|> constTrue
       <|> ifthenelse
       <|> iszero
       <|> numeral
       <|> nsucc
       <|> abstraction
       <|> variable
       
expr = do
  exprlst <- many1 term
  return (foldl1 SApp exprlst)

program p = do
  whiteSpace
  r <- p
  eof
  return r

run p input = parse (program p) "" input
