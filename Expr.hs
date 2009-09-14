module Expr
    (
     RepExpr(..)
    ,RepVar
    ,Context
    ,contextResult
    ,contextFromList
    ,evaluer
    ) where

import qualified Data.Map as Map

type RepVar = Integer

type Context = Map.Map RepVar Integer

data RepExpr = RepAff RepVar RepVar
             | RepInc RepVar  
             | RepRep RepVar [RepExpr]

-- pretty printing functions

instance Show RepExpr where
    show (RepAff a b) = "r" ++ show a ++ " <- r" ++ show b
    show (RepInc a)   = "inc(r" ++ show a ++ ")"
    show (RepRep a b) = "repeter r" ++ show a ++ " [" ++ showRepList b ++ "]"

showRepList :: [RepExpr] -> String
showRepList [] = "\n\t"
showRepList (x:xs) = "\n\t" ++ show x  ++ showRepList xs

-- evaluation functions

evaluer :: Context -> RepExpr -> Context
evaluer ctx (RepInc var)         = contextSet ctx var (1 + contextGet ctx var)
evaluer ctx (RepAff dest source) = contextSet ctx dest (contextGet ctx source)
evaluer ctx (RepRep var block)   = evaluerBlockNFois ctx (contextGet ctx var) block 

evaluerBlockNFois :: Context -> Integer -> [RepExpr] -> Context
evaluerBlockNFois ctx 0 _ = ctx
evaluerBlockNFois ctx n block = 
    evaluerBlockNFois (evaluerBlock ctx block) (n - 1) block

evaluerBlock :: Context -> [RepExpr] -> Context
evaluerBlock ctx [] = ctx
evaluerBlock ctx (x:xs) = evaluerBlock (evaluer ctx x) xs

-- context manipulation functions

contextGet :: Context -> RepVar -> Integer
contextGet ctx var = Map.findWithDefault 0 var ctx

contextSet :: Context -> RepVar -> Integer -> Context
contextSet ctx var val = Map.insert var val ctx

contextResult :: Context -> Integer
contextResult ctx = contextGet ctx 0 

contextFromList :: [Integer] -> Context
contextFromList l = buildContext l (Map.empty::Context) 0
    where
      buildContext [] c _ = c
      buildContext (x:xs) c lvl = buildContext xs (contextSet c lvl x) (lvl + 1)

