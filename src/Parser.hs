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

numeral = natural >>= \n -> return (Num n)

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

app =
  do
    exprlst <- many1 term
    return (foldl1 SApp exprlst)

expr = buildExpressionParser table app
  where op x f = Infix (reservedOp x >> return f)
        table = [[op "**" (Bop Pow) AssocRight],
                 [op "*" (Bop Mult) AssocLeft],
                 [op "+" (Bop Plus) AssocLeft,
                  op "-" (Bop Minus) AssocLeft],
                 [op "&&" (Boolop And) AssocLeft],
                 [op "||" (Boolop Or) AssocLeft],
                 [op "<" (Rop Lt) AssocNone,
                  op "<=" (Rop Leq) AssocNone,
                  op "==" (Rop Eq) AssocNone,
                  op ">=" (Rop Geq) AssocNone,
                  op ">" (Rop Gt) AssocNone]]

program p =
  do
    whiteSpace
    r <- p
    eof
    return r

parse p input = Parsec.parse (program p) "" input
