module Eval where

import Types


substitute :: (String, Term)  -> Term -> Term
substitute (id, e) (Ident x)
  | id == x = e
  | otherwise = Ident x
substitute s (App e1 e2) =
  App (substitute s e1) (substitute s e2)
substitute (id, e1) (Abs x e2)
  | id == x = Abs x e2
  | otherwise = Abs x (substitute (id, e1) e2)

isValue (Abs _ _) = True
isValue (App _ _) = False

evalByVal :: Term -> Either String Term
evalByVal (App abs@(Abs id e1) e2)
  | isValue e2 = Right (substitute (id, e2) e1) 
  | otherwise  =
    case evalByVal e2 of
      Left str -> Left str
      Right e2' -> Right (App abs e2')
evalByVal (App e1 e2) =
  case evalByVal e1 of
    Left str -> Left str
    Right e1' -> Right (App e1' e2)
evalByVal _ =
  Left "Serious Internal Error"

isValueN (Abs id e) = isValueN e
isValueN (Ident _) = True
isValueN (App (Abs _ _) e2) = False
isValueN (App e1 e2) = (isValueN e1) && (isValueN e2)

evalNormal :: Term -> Either String Term
evalNormal (Abs id e1) 
  | isValueN e1 = Left "Serious internal error 1"
  | otherwise =
    case evalNormal e1 of
      Right e1' -> Right (Abs id e1')
      Left str -> Left str
evalNormal (App (Abs id e1) e2) =
  Right (substitute (id, e2) e1) 
evalNormal (App e1 e2) =
  if (isValueN e1) then
    case evalNormal e2 of
      Left str -> Left str
      Right e2' -> Right (App e1 e2')
  else
    case evalNormal e1 of
      Left str -> Left str
      Right e1' -> Right (App e1' e2)
evalNormal _ =
  Left "Serious Internal Error 2"

interpret :: (Term -> Either String Term) -> (Term -> Bool) -> Term -> Either String Term
interpret f value expr 
  | value expr = Right expr
  | otherwise    =
    case f expr of
      Left str -> Left str
      Right expr' -> interpret f value expr'

normalOrder term = interpret evalNormal isValueN term
