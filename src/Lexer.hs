module Lexer where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token as T
import Text.Parsec.Language

def = haskellDef{ T.reservedOpNames = ["\\", ".", "=", "+", "-", "*", "**", "<",
                                       "<=", ">", ">=", "==", "&&", "||"]
                , T.reservedNames = ["let", "in", "true",
                                     "false", "if", "then",
                                     "else", "let rec"]
                }

lexer :: TokenParser ()
lexer = makeTokenParser def

whiteSpace = T.whiteSpace lexer
lexeme     = T.lexeme lexer
natural    = T.natural lexer
parens     = T.parens lexer
brackets   = T.brackets lexer
comma      = T.comma lexer 
identifier = T.identifier lexer
reserved   = T.reserved lexer
reservedOp = T.reservedOp lexer

