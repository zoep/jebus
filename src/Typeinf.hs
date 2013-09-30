module Typeinf where

import Types
import qualified Data.Set as Set
import Data.List

fresh :: FreshPool -> (TypeVar, FreshPool)
fresh = \pool -> (head pool, tail pool)

substType :: TypeSubst -> Type -> Type 
substType Id tau = tau
substType (Sub a1 tau) (Tvar a2)
  | a1 == a2 = tau
  | otherwise = Tvar a2
substType (Sub a tau) (Arrow tau1 tau2) =
  Arrow (substType (Sub a tau) tau1) (substType (Sub a tau) tau2)
substType (Sub _ _) tau = tau  
substType (Composition s1 s2) tau =
  substType s1 (substType s2 tau)

substScheme :: TypeSubst -> TypeScheme -> TypeScheme
substScheme Id scheme = scheme
substScheme (Composition s1 s2) scheme =
  substScheme s1 (substScheme s2 scheme)
substScheme s (SimpleType tau) = SimpleType (substType s tau)
substScheme (Sub a1 tau) (ForEach a2 scheme)
  | a1 == a2 = ForEach a2 scheme
  | otherwise = ForEach a2 (substScheme (Sub a1 tau) scheme)
  
substEnv :: TypeSubst -> TypeEnv -> TypeEnv
-- consider using map here (note to ourselves)
substEnv sub [] = []
substEnv sub ((term, scheme) : rest) =
  (term, substScheme sub scheme) : (substEnv sub rest)

notOccurs :: TypeVar -> Type -> Bool
notOccurs a1 (Tvar a2) = a1 /= a2
notOccurs a (Arrow tau1 tau2) = (notOccurs a tau1) && (notOccurs a tau2)
notOccurs a _ = True

unify :: Type -> Type -> Either String TypeSubst 
unify tau1 tau2
  | tau1 == tau2 = Right Id
unify (Tvar a1) tau2
  | notOccurs a1 tau2 = Right (Sub a1 tau2)
  | otherwise =
    Left $ "Occurs check: cannot construct the infinite type: " ++ ppType tau2
    ++ "=" ++ ppType (Tvar a1)
unify tau1 (Tvar a2)
  | notOccurs a2 tau1 = Right (Sub a2 tau1)
  | otherwise =
    Left $ "Occurs check: cannot construct the infinite type: " ++ ppType tau1
    ++ "=" ++ ppType (Tvar a2)
unify (Arrow tau11 tau12) (Arrow tau21 tau22) =
  do
    subst1 <- unify tau11 tau21
    subst2 <- unify (substType subst1 tau12) (substType subst1 tau22)
    return $ Composition subst1 subst2
unify (Pair tau11 tau12) (Pair tau21 tau22) =
   do
    subst1 <- unify tau11 tau21
    subst2 <- unify (substType subst1 tau12) (substType subst1 tau22)
    return $ Composition subst1 subst2 
unify tau1 tau2 =
  Left $ "Could not match type " ++ ppType tau1 ++ " with type " ++ ppType tau2


inst :: TypeScheme -> FreshPool -> (Type, FreshPool)
inst (SimpleType tau) pool  = (tau, pool)
inst (ForEach a scheme) pool =
  let (a', pool') = fresh pool in
  inst (substScheme (Sub a (Tvar a')) scheme) pool'

freeTypeVariables :: Type -> Set.Set TypeVar
freeTypeVariables tau = aux tau Set.empty
  where
    aux (Tvar a) set = Set.insert a set
    aux (Arrow tau1 tau2) set =
      let set' = aux tau1 set in
      aux tau2 set'
    aux _ set = set

freeSchemeVariables :: TypeScheme -> Set.Set TypeVar
freeSchemeVariables (SimpleType tau) = freeTypeVariables tau
freeSchemeVariables (ForEach a scheme) =
  (freeSchemeVariables scheme) Set.\\ Set.singleton a

freeEnvVariables :: TypeEnv -> Set.Set TypeVar
freeEnvVariables env = Set.unions $
  foldr (\(_, scheme) setlst ->
          (freeSchemeVariables scheme) : setlst) [] env

gen :: TypeEnv -> Type -> TypeScheme
gen env tau =
  let fv = Set.difference (freeTypeVariables tau) (freeEnvVariables env) in
  Set.foldr (\tvar scheme -> ForEach tvar scheme) (SimpleType tau) fv
  
inDom :: STerm -> TypeEnv -> Maybe TypeScheme 
inDom term env = lookup term env
