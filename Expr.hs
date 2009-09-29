{-    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Expr
    (
     RepExpr(..)
    ,RepVar
    ,Context
    ,contextResult
    ,contextFromList
    ,contextGet -- really export these 3 vars?
    ,contextSet
    ,contextGetFreeVar
    ,evaluer
    ,evaluerBlock
    ) where

import qualified Data.Map as Map
import Control.Monad.State

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

{-
type EvalState = State Context

ev :: RepExpr -> EvalState ()
ev (RepInc var) = do
  ctx <- get
  put (contextSet ctx var (1 + contextGet ctx var))
-}

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

contextGetFreeVar :: Context -> RepVar
contextGetFreeVar ctx = 1 + maximum (Map.keys ctx)