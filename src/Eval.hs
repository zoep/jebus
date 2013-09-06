module Eval where

import Types

-- Checks if an identifier occurs free in a term

isFreeIn :: String -> Term -> Bool
isFreeIn id (Ident x) = x == id
isFreeIn id (App e1 e2) = (isFreeIn id e1) || (isFreeIn id e2)
isFreeIn id (Abs x e) 
  | id == x = False
  | otherwise = isFreeIn id e
isFreeIn id (Fix e) = isFreeIn id e
  
-- Concatenates an identifier with the next available number

freshName :: FreshPool -> String -> (String, FreshPool)
freshName pool id = (id ++ show (head pool), tail pool)

substitute :: (String, Term)  -> Term -> FreshPool -> (Term, FreshPool)
substitute (id, e) (Ident x) fresh
  | id == x = (e, fresh)
  | otherwise = (Ident x, fresh)
substitute s (App e1 e2) fresh =
  let (t1, fresh') = substitute s e1 fresh in
  let (t2, fresh'') = substitute s e2 fresh' in
    (App t1 t2, fresh'')
substitute (id, e1) (Abs x e2) fresh 
  | id == x = (Abs x e2, fresh)
  | (isFreeIn x e1) && (isFreeIn id e2) =
    let (newx, fresh') = freshName fresh x in
    let (e2', fresh'') = substitute (x, Ident newx) e2 fresh' in 
    let (t, fresh''') = substitute (id, e1) e2' fresh'' in
      (Abs newx t, fresh''')
  | otherwise = 
     let (t, fresh') = substitute (id, e1) e2 fresh in
       (Abs x t, fresh')
substitute (id, e1) (Fix e2) fresh =
  let (e2', fresh') = substitute (id, e1) e2 fresh in
    (Fix e2', fresh')


isValueBV (Abs _ _) = True
isValueBV (App _ _) = False

evalByVal :: Term -> FreshPool -> Either String (Term, FreshPool)
evalByVal (App abs@(Abs id e1) e2) fresh
  | isValueBV e2 = return (substitute (id, e2) e1 fresh) 
  | otherwise  =
    do 
      (e2', fresh') <- evalByVal e2 fresh
      return (App abs e2', fresh')
evalByVal (App e1 e2) fresh =
    do 
      (e1', fresh') <- evalByVal e2 fresh
      return (App e1' e2, fresh')
evalByVal _ _ =
  Left "Serious Internal Error"

isValueN (Abs id e) = isValueN e
isValueN (Ident _) = True
isValueN (App (Abs _ _) e2) = False
isValueN (Fix e) = False --isValueN e
isValueN (App e1 e2) = (isValueN e1) && (isValueN e2)

evalNormal :: Term -> FreshPool -> Either String (Term, FreshPool)
evalNormal (Abs id e1) fresh
  | isValueN e1 = Left "Serious internal error 1"
  | otherwise =
    do 
      (e1', fresh') <- evalNormal e1 fresh
      return $ (Abs id e1', fresh')
evalNormal (App (Abs id e1) e2) fresh =
  return $ substitute (id, e2) e1 fresh
evalNormal (App e1 e2) fresh =
  if (isValueN e1) then
    do 
      (e2', fresh') <- evalNormal e2 fresh
      return (App e1 e2', fresh')
  else
    do
      (e1', fresh') <- evalNormal e1 fresh
      return (App e1' e2, fresh')
evalNormal (Fix (Abs id e)) fresh =
  return $ substitute (id, Fix (Abs id e)) e fresh
evalNormal (Fix e) fresh =
  do
    (e', fresh') <- evalNormal e fresh
    return (Fix e', fresh')
evalNormal _ _ =
  Left "Serious Internal Error 2"

interpret :: (Term -> FreshPool -> Either String (Term, FreshPool)) -> (Term -> Bool) -> Term -> FreshPool -> Either String Term
interpret f value expr fresh
  | value expr = return expr
  | otherwise    = 
      f expr fresh >>= \(expr', fresh') ->
      interpret f value expr' fresh'

normalOrder term = interpret evalNormal isValueN term [1..]

