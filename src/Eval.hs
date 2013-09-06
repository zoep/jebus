module Eval where

import Types

-- Checks if identifier id occurs free in a term

isFreeIn :: String -> Term -> Bool
isFreeIn id (Ident x) = x == id
isFreeIn id (App e1 e2) = (isFreeIn id e1) || (isFreeIn id e2)
isFreeIn id (Abs x e) 
  | id == x = False
  | otherwise = isFreeIn id e

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
  | isValue e2 = return (substitute (id, e2) e1) 
  | otherwise  =
    do 
      e2' <- evalByVal e2
      return $ App abs e2'
evalByVal (App e1 e2) =
    do 
      e1' <- evalByVal e2 
      return $ App e1' e2
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
    do 
      e1' <- evalNormal e1
      return $ Abs id e1'
evalNormal (App (Abs id e1) e2) =
  return $ substitute (id, e2) e1 
evalNormal (App e1 e2) =
  if (isValueN e1) then
    do 
      e2' <- evalNormal e2
      return $ App e1 e2'
  else
    do
      e1' <- evalNormal e1
      return $ App e1' e2
evalNormal _ =
  Left "Serious Internal Error 2"

interpret :: (Term -> Either String Term) -> (Term -> Bool) -> Term -> Either String Term
interpret f value expr 
  | value expr = Right expr
  | otherwise    = 
      f expr >>= \expr' ->
      interpret f value expr'

normalOrder term = interpret evalNormal isValueN term

