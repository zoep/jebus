module Eval where
import qualified Data.List as List
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

isValue (Abs id e) = isValue e
isValue (Ident _) = True
isValue (App (Abs _ _) e2) = False
isValue (Fix e) = False --isValue e
isValue (App e1 e2) = (isValue e1) && (isValue e2)


evalApplicative :: Term -> FreshPool -> Either String (Term, FreshPool)
evalApplicative e@(Abs id e1) fresh
  | isValue e1 = Left $ "Serious internal error, umatched " ++ ppTerm e
  | otherwise =
    do 
      (e1', fresh') <- evalApplicative e1 fresh
      return $ (Abs id e1', fresh')
evalApplicative (App e1 e2) fresh | not (isValue e1 && isValue e2) =
  if (isValue e1) then
    do
      (e2', fresh') <- evalApplicative e2 fresh
      return (App e1 e2', fresh')
  else
    do
      (e1', fresh') <- evalApplicative e1 fresh
      return (App e1' e2, fresh')
evalApplicative (App (Abs id e1) e2) fresh = -- will reach that rule only if e1 and e2 are values!
    return $ substitute (id, e2) e1 fresh
evalApplicative (Fix (Abs id e)) fresh =
  return $ substitute (id, Fix (Abs id e)) e fresh
evalApplicative (Fix e) fresh =
  do
    (e', fresh') <- evalApplicative e fresh
    return (Fix e', fresh')
evalApplicative e1 _ =
  Left $ "Serious internal error 2, umatched " ++ ppTerm e1 


evalNormal :: Term -> FreshPool -> Either String (Term, FreshPool)
evalNormal e@(Abs id e1) fresh
  | isValue e1 = Left $ "Serious internal error 1, umatched " ++ ppTerm e
  | otherwise =
    do 
      (e1', fresh') <- evalNormal e1 fresh
      return $ (Abs id e1', fresh')
evalNormal (App (Abs id e1) e2) fresh =
  return $ substitute (id, e2) e1 fresh 
evalNormal (App e1 e2) fresh =
  if (isValue e1) then
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
evalNormal e1 _ =
  Left $ "Serious internal error 2, umatched " ++ ppTerm e1 

interpret :: (Term -> FreshPool -> Either String (Term, FreshPool)) ->
             (Term -> Bool) -> Term -> FreshPool -> Either String Term
interpret eval isValue expr fresh
  | isValue expr = return expr
  | otherwise    = 
      eval expr fresh >>= \(expr', fresh') ->
      interpret eval isValue expr' fresh'


interpretT :: [Term] -> (Term -> FreshPool -> Either String (Term, FreshPool)) ->
              (Term -> Bool) -> Term -> FreshPool -> Either String [Term]
interpretT acc eval isValue expr fresh
  | isValue expr = return $ List.reverse (expr : acc)
  | otherwise = 
      eval expr fresh >>= \(expr', fresh') ->
      interpretT (expr : acc) eval isValue expr' fresh'

normalOrder term = interpret evalNormal isValue term [1..]

normalOrderT term = interpretT [] evalNormal isValue term [1..]

applicativeOrder term = interpret evalApplicative isValue term [1..]

applicativeOrderT term = interpretT [] evalApplicative isValue term [1..]
