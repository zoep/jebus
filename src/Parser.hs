module Parser where

import Text.Parsec as Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Lexer
import Types

abstraction =
  do
    reservedOp "\\"
    v <- identifier
    reservedOp "."
    t <- expr
    return (SAbs v t)
    
variable = identifier >>= \v -> return (SId v)
    
ifthenelse =
  do
    reserved "if"
    cond <- expr
    reserved "then"
    e1 <- expr
    reserved "else"
    e2 <- expr
    return $ IfThenElse cond e1 e2
                 
constTrue = reserved "true" >> return (Boolean True)
                
constFalse = reserved "false" >> return (Boolean False)


--iszero = reserved "iszero" >> return IsZero

numeral = natural >>= \n -> return (Num n)

{-
nsucc = reserved "succ" >> return Succ

prev = reserved "pred" >> return Pred
          
first = reserved "fst" >> return Fst
  
second = reserved "snd" >> return Snd
-}

inpair =
  do
    e1 <- expr
    comma
    e2 <- expr
    return $ SPair e1 e2
            
pair = brackets inpair

letin =
  do
    reserved "let"
    v <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ LetIn v e1 e2
    
letrecin =
  do
    reserved "let rec"
    v <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return (LetRec v e1 e2)

term = parens expr
       <|> letrecin
       <|> letin
       <|> constFalse
       <|> constTrue
       <|> ifthenelse
       <|> numeral
       <|> pair
       <|> abstraction
       <|> variable
       
oper = buildExpressionParser table term
  where op x f = Infix (reservedOp x >> return f)
        table = [[op "**" Pow AssocRight],
                 [op "*" Mult AssocLeft ],
                 [op "+" Plus AssocLeft,
                  op "-" Minus AssocLeft]]


expr =
  do
    exprlst <- many1 oper
    return (foldl1 SApp exprlst)

program p =
  do
    whiteSpace
    r <- p
    eof
    return r

parse p input = Parsec.parse (program p) "" input
