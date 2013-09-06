module Ast where

import Types
import qualified Typeinf as T

intToChurch n =
  (Abs "f" (Abs "x" (appN n)))
  where
    appN n = iterate (\x -> App (Ident "f") x) (Ident "x") !! n

boolToChurch True = Abs "x" (Abs "y" (Ident "x"))
boolToChurch False = Abs "x" (Abs "y" (Ident "y"))

infer ast =
  walk ast [] [1..]

walk :: STerm -> TypeEnv -> FreshPool -> Either String Node
walk (SId x) env pool =
  case (T.inDom (SId x) env) of
    Just scheme ->
      let (tau, pool') = T.inst scheme pool in
      Right $ Node {
        nodeExpr = Ident x,
        tExpr = TId x,
        typ = tau,
        subst = Id,
        pool = pool'}
    Nothing -> Left $ "Unbound identifier " ++ x

walk (SAbs id term) env pool =
  do
    let (a, pool') = T.fresh pool 
    node <- walk term ((SId id, SimpleType (Tvar a)) : env) pool'
    let idTau = T.substType (subst node) (Tvar a)
    let tau = Arrow idTau (typ node)
    return $ node {
      nodeExpr = Abs id (nodeExpr node),
      tExpr = TAbs (id, idTau) (tExpr node, tau),
      typ = tau 
      }

walk (SFix term) env p =
  do
    let (a, p') = T.fresh p  -- maybe returning (Tvar a) is a better design choice
    node <- walk term env p'
    sub <- T.unify (typ node) (Arrow (Tvar a) (Tvar a))
    return $ node {
      nodeExpr = Fix (nodeExpr node),
      tExpr = TFix (tExpr node),
      typ = T.substType sub (Tvar a)
      }

walk (SApp term1 term2) env p =
  do 
    let (a, p') = T.fresh p
    node1 <- walk term1 env p'
    node2 <- walk term2 (T.substEnv (subst node1) env) (pool node1) 
    sub <- T.unify (T.substType (subst node2) (typ node1)) (Arrow (typ node2) (Tvar a))
    return $ node2 {
      nodeExpr = App (nodeExpr node1) (nodeExpr node2),
      tExpr = TApp (tExpr node1) (tExpr node2),
      typ = T.substType sub (Tvar a),
      subst = Composition sub (Composition (subst node2) (subst node1))
      }

walk (LetIn id term1 term2) env p =
  do
    node1 <- walk term1 env p
    let env' = T.substEnv (subst node1) env 
    node2 <- walk term2 ((SId id, T.gen env' (typ node1)) : env') (pool node1)
    return $ node2 {
      nodeExpr = App (Abs id (nodeExpr node2)) (nodeExpr node1),
      tExpr = TLetIn (id, (typ node1)) (tExpr node1) (tExpr node2),
      subst = Composition (subst node2) (subst node1)
      }

walk (Num n) env p =
  do
    return $ Node {
      nodeExpr = intToChurch (fromIntegral n),
      tExpr = TNum n,
      typ = Nat,
      subst = Id,
      pool = p
      }

walk (Boolean bool) env p =
   Right $ Node {
    nodeExpr = boolToChurch bool,
    tExpr = TBoolean bool,
    typ = Bool,
    subst = Id,
    pool = p
    }

walk IsZero env p =
  Right $ Node {
    nodeExpr = Abs "n" (App (App (Ident "n") (Abs "x" (boolToChurch False))) (boolToChurch True)),
    tExpr = TIsZero,
    typ = Arrow Nat Bool,
    subst = Id,
    pool = p
    }

walk Succ env p =
  Right $ Node {
    nodeExpr = Abs "n" (Abs "s" (Abs "z" (App (Ident "s") (App (App (Ident "n") (Ident "s")) (Ident "z"))))) ,
    tExpr = TSucc,
    typ = Arrow Nat Nat,
    subst = Id,
    pool = p
    }



walk (IfThenElse cond term1 term2) env p = 
  do
    condNode <- walk cond env p
    sub1 <- T.unify (typ condNode) Bool
    let env' = T.substEnv (Composition sub1 (subst condNode)) env 
    node1 <- walk term1 env' (pool condNode)
    let env'' = T.substEnv (subst node1) env'
    node2 <- walk term2 env'' (pool node1)
    sub2 <- T.unify (T.substType (subst node2) (typ node1)) (typ node2)
    let fsub = foldr (\s prev -> Composition s prev) (subst condNode) [sub2, subst node2, subst node1, sub1]
    return $ node2 {
      nodeExpr = App (App (nodeExpr condNode) (nodeExpr node1)) (nodeExpr node2),
      tExpr = TIfThenElse (tExpr condNode) (tExpr node1) (tExpr node2),
      typ = T.substType sub2 (typ node2),
      subst = fsub
      }
