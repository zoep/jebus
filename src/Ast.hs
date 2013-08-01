module Ast where

data Term = Abs String Term
          | App Term Term
          | Id String 
          deriving (Show) 

