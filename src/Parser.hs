module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Lexer
import Ast

intToChurch n =
  (Abs "f" (Abs "x" (appN n)))
  where
    appN n = iterate (\x -> App (Id "f") x) (Id "x") !! n

true = Abs "x" (Abs "y" (Id "x"))
false = Abs "x" (Abs "y" (Id "y"))

abstraction = do { reservedOp "\\"
                 ; v <- identifier
                 ; reservedOp "."
                 ; t <- expr
                 ; return (Abs v t)
                 }

variable = do { v <- identifier
              ; return (Id v)
              }

ifthenelse = do { reserved "if"
                ; cond <- expr
                ; reserved "then"
                ; e1 <- expr
                ; reserved "else"
                ; e2 <- expr
                ; return (App (App cond e1) e2)
                }
             
constTrue = do { reserved "true"
               ; return true 
               }
            
constFalse = do { reserved "false"
                ; return false 
                }

iszero = do { reserved "iszero"
            ; return (Abs "n" (App (App (Id "n") (App true false)) true))
            }

numeral = do { v <- natural
             ; return (intToChurch (fromIntegral v))
             }

nsucc = do { reserved "succ"
           ; return (Abs "z" (Abs "f" (Abs "x" (App (App (Id "z") (Id "f")) (Id "x")))))
           }

letin = do { reserved "let"
           ; v <- identifier
           ; reservedOp "="
           ; e1 <- expr
           ; reserved "in"
           ; e2 <- expr
           ; return (App (Abs v e2) e1)
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
  return (foldl1 App exprlst)

program p = do
  whiteSpace
  r <- p
  eof
  return r

run p input
        = case (parse (program p) "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
