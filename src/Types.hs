module Types where
import Text.PrettyPrint as PP
import Control.Monad.Instances

type TypeVar = Int 

type FreshPool = [TypeVar]
{-
instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= _ = Left e-}

data Type = Arrow Type Type
          | Nat
          | Bool
          | Tvar TypeVar
          deriving (Eq, Show)


ppType :: Type -> String
ppType t = PP.render (aux t False)
  where aux (Arrow t1 t2) c =
          (if c then parens else id)  
          ((aux t1 True) <> PP.text "->" <> (aux t2 False))
        aux Nat _ = PP.text "Nat"
        aux Bool _ = PP.text"Bool"
        aux (Tvar a) _ = PP.text ("a" ++ show a) 
    
data TypeScheme = SimpleType Type
                | ForEach TypeVar TypeScheme
                deriving (Show, Eq)
                       
type TypeEnv = [(STerm, TypeScheme)]

data TypeSubst = Id
               | Sub TypeVar Type
               | Composition TypeSubst TypeSubst

-- Datatype for the sugared AST
data STerm = SAbs String STerm
           | SApp STerm STerm
           | SId String
           | IfThenElse STerm STerm STerm
           | Num Integer
           | Boolean Bool
           | LetIn String STerm STerm
           | LetRec String STerm STerm
           | IsZero
           | Succ
           deriving (Show, Eq) 

data TypedSTerm = TAbs (String, Type) (TypedSTerm, Type)
                | TFix TypedSTerm
                | TApp TypedSTerm TypedSTerm
                | TId String
                | TIfThenElse TypedSTerm TypedSTerm TypedSTerm
                | TNum Integer
                | TBoolean Bool
                | TLetIn (String, Type) TypedSTerm TypedSTerm
                | TLetRec (String, Type) TypedSTerm TypedSTerm
                | TIsZero
                | TSucc
                deriving Eq

ppTTerm :: TypedSTerm -> String
ppTTerm term = PP.render (aux term 0 0)
  where aux :: TypedSTerm -> Int -> Int -> Doc
        aux (TId x) _ _ = PP.text x
        aux (TAbs (x, t1) (e, t2)) n c = PP.nest n $
          (if c > 0 then parens else id)
          ((PP.text ("\\" ++ x ++ " : " ++ ppType t1 ++ " . ")) <>
           (aux e n 0))
        aux (TApp e1 e2) n c = PP.nest n $
          (if c > 1 then PP.parens else id)
          (aux e1 n 1 <+> aux e2 n 2)
        aux (TIfThenElse e1 e2 e3) n _ = PP.nest n $
          PP.text "if" <+> aux e1 n 2 $+$
          PP.text "then" <+> aux e2 n 2 $+$
          PP.text "else" <+> aux e3 n 2
        aux (TNum n) _ _ = PP.integer n
        aux (TBoolean b) _ _ = boolean b
        aux (TLetIn (id, t) e1 e2) n c = PP.nest n $
          PP.text ("let " ++ id ++ " : " ++ ppType t ++ "=") $+$
          aux e1 (n+1) c $$
          PP.text "in" $$
          aux e2 (n+1) c
        aux (TLetRec (id, t) e1 e2) n c = PP.nest n $
          PP.text ("let rec " ++ id ++ " : " ++ ppType t ++ "=") $+$
          aux e1 (n+1) c $$
          PP.text "in" $$
          aux e2 (n+1) c
        aux TIsZero _ _ = PP.text "iszero"
        aux TSucc _ _ = PP.text "succ"
        --aux (TFix e) n c = PP.nest n $ parens (PP.text "fix" <+> aux e n c) 
        boolean True = text "true"
        boolean False = text "false"
        
    
-- Datatype for the desugared AST
data Term = Abs String Term
          | Fix Term
          | App Term Term
          | Ident String 
          deriving Eq

ppTerm :: Term -> String
ppTerm term = PP.render $ aux term 0
  where
    aux (Ident id) _ = PP.text id 
    aux (Abs x e) c =
      (if c > 0 then parens else id)
      ((PP.text ("\\" ++ x ++ " . ")) <>
       (aux e 0))
    aux (App e1 e2) c = 
      (if c > 1 then PP.parens else id)
      (aux e1 1 <+> aux e2 2)
    aux (Fix e) c = PP.parens $
      PP.text "fix" <+> aux e c
    
data Node = Node {
  nodeExpr  :: Term,
  tExpr     :: TypedSTerm,
  typ       :: Type,
  subst     :: TypeSubst,
  pool      :: FreshPool
  }
