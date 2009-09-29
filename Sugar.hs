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

import Expr
import qualified Data.List as List
import Control.Monad.State
--import qualified Data.Map as Map

data SugLit = SVar RepVar | SNum Integer deriving (Show) 


data SugarExpr = SugarAff RepVar SugLit
               | SugarInc RepVar
               | SugarApp  ProcDef [SugLit] RepVar -- proc name, parameters, result
               | SugarRep SugLit [SugarExpr]
                 deriving (Show)


--               ProcDef Name   Param    Exprs
data ProcDef = ProcDef String [RepVar] [SugarExpr] deriving (Show)

procParams (ProcDef _ params _) = params
procExprs  (ProcDef _ _ exprs) = exprs

type SugarState = State Context

example = ProcDef "proc" [1,2,3] [SugarInc 1, SugarInc 5, SugarInc 0]
varRep = calculateVarReplacement example [111, 222, 333] (contextFromList[0,0,0,0,0,4])


ex2 = adaptProc example [111, 222, 333] (contextFromList[0,0,0,0,0,4])
ex3 = adaptProc example [111, 222, 333] (contextFromList[0,0,0,0,0,0])

addition = ProcDef "PLUS" [1, 2] [SugarAff 0 (SVar 1), SugarRep (SVar 2) [SugarInc 0]]

application = [SugarInc 0, SugarApp addition [(SNum 5), (SNum 14)] 2, SugarAff 0 (SVar 2)]

test1 = removeProcApplications (contextFromList[0]) application
test2 = convertToExpr application (contextFromList[0])

test3 = evaluerBlock ctx exprs
    where
      (exprs, ctx) = convertToExpr application (contextFromList[0])

convertToExpr :: [SugarExpr] -> Context -> ([RepExpr], Context)
convertToExpr exprs ctx = ((map convert exprs'), ctx')
    where
      (exprs', ctx') = removeProcApplications ctx exprs
      convert :: SugarExpr -> RepExpr
      convert (SugarAff v1 (SVar v2)) = RepAff v1 v2
      convert (SugarInc v1) = RepInc v1
      convert (SugarRep (SVar v1) exs) = RepRep v1 (map convert exs)
      convert _ = error "Non trivial expresion conversion found"
      

removeProcApplications :: Context -> [SugarExpr] -> ([SugarExpr], Context)
removeProcApplications ctx exprs = ((concatMap removeApplication exprs'), ctx')
    where
      (exprs', ctx') = prepareProcApplications ctx exprs
      removeApplication :: SugarExpr -> [SugarExpr]
      removeApplication (SugarApp (ProcDef _ _ exs) _ ret) = 
          concatMap removeApplication $ map (changeVarFromList [(0, ret)]) exs
      removeApplication e = [e]


-- this should return the context augmente with the removed literals
prepareProcApplications :: Context -> [SugarExpr] -> ([SugarExpr], Context)
prepareProcApplications ctx exprs = (map applyProc exprs', ctx')
    where
      (exprs', ctx') = removeLiterals exprs (populateContext exprs ctx)
      applyProc :: SugarExpr -> SugarExpr
      applyProc (SugarApp proc params ret) = 
          SugarApp (adaptProc proc (litToVars params) ctx') params ret
      applyProc e = e

      litToVars :: [SugLit] -> [RepVar]
      litToVars lits = map litToVar lits

      litToVar :: SugLit -> RepVar
      litToVar (SVar n) = n
      litToVar (SNum _) = error "Not supposed to have literals at this point"

-- adds to the context all the refered variables of the list of expresions
populateContext :: [SugarExpr] -> Context -> Context
populateContext exprs = execState (do mapM addVar exprs)
    where
      addVar :: SugarExpr -> State Context ()
      addVar (SugarAff v1 (SVar v2)) = do incInCtx v1; incInCtx v2; return ()
      addVar (SugarAff v1 (SNum  _)) = do incInCtx v1; return ()
      addVar (SugarInc v1) = do incInCtx v1; return ()
      addVar (SugarRep (SVar v1) exps) = do incInCtx v1; mapM addVar exps; return ()
      addVar (SugarRep (SNum  _) exps) = do mapM addVar exps; return ()
      addVar (SugarApp _ lits v1) = do incInCtx v1; mapM incLit lits; return ()

      incInCtx :: RepVar -> State Context ()
      incInCtx var = do 
        ctx <- get
        put $ contextSet ctx var (contextGet ctx var)
      
      incLit :: SugLit -> State Context ()
      incLit (SNum _) = return ()
      incLit (SVar v) = do incInCtx v; return ()
        

--           proc       params      context    modifiedProcDef
adaptProc :: ProcDef -> [RepVar] -> Context -> ProcDef
adaptProc (ProcDef  name params exprs) newParams ctx = ProcDef name newParams newExprs
    where
      newExprs = map (changeVarFromList varRep) exprs
      varRep = calculateVarReplacement (ProcDef name params exprs) newParams ctx


calculateVarReplacement :: ProcDef -> [RepVar] -> Context -> [(RepVar, RepVar)]
calculateVarReplacement proc newParams ctx = map replaceVar allVars
    where
      allVars = usedVariables proc
      params = procParams proc
      replaceVar :: RepVar -> (RepVar, RepVar)
      replaceVar v | v == 0 = (0, 0)
                   | v `elem` params = (v, newParams 
                                        !! ((v `List.elemIndices` params) !! 0))
                   | v `elem` allVars = 
                       (v,   --horrible hack! replace with a state monad
                        (contextGetFreeVar ctx) + toInteger ((v `List.elemIndices` allVars) !! 0))
                         
                   | otherwise = error "Variable not found"


usedVariables :: ProcDef -> [RepVar]
usedVariables (ProcDef name params exprs) =
    List.nub ((concat (map exprVars exprs)) ++ (params))
        where
          exprVars :: SugarExpr -> [RepVar]
          exprVars (SugarAff v1 (SVar v2)) = List.nub [v1, v2]
          exprVars (SugarAff v1 (SNum  _)) = [v1]

          exprVars (SugarInc v1) = [v1]

          exprVars (SugarRep (SVar v1) exprs) = List.nub ([v1] ++ (concat (map exprVars exprs)))
          exprVars (SugarRep (SNum n1) exprs) = List.nub (concat (map exprVars exprs))

          exprVars (SugarApp _ params v1) = List.nub ([v1] ++ (getVars params))
                                            
          getVars :: [SugLit] -> [RepVar]
          getVars [] = []
          getVars (x:xs) = (onlySVar x) ++ getVars xs
          onlySVar (SNum n) = []
          onlySVar (SVar v) = [v]


changeVarFromList :: [(RepVar, RepVar)] -> SugarExpr -> SugarExpr
changeVarFromList list (SugarInc v) = (SugarInc (findVarReplacement v list))

changeVarFromList list (SugarAff v1 (SVar v2)) = 
    SugarAff (findVarReplacement v1 list) (SVar (findVarReplacement v2 list))

changeVarFromList list (SugarAff v1 (SNum n1)) = 
    SugarAff (findVarReplacement v1 list) (SNum n1)

changeVarFromList list (SugarRep (SVar v1) exprs) = 
    SugarRep (SVar (findVarReplacement v1 list)) (map (changeVarFromList list) exprs)


findVarReplacement :: RepVar -> [(RepVar, RepVar)] -> RepVar
findVarReplacement v reps = case (lookup v reps) of
                              Just rv -> rv
                              Nothing -> v

{-
l = [SugarRep (SVar 121) [SugarAff 1 (SNum 14), SugarAff 1 (SNum 34)]]
res = removeLiterals l (contextFromList [0, 2])
-}

removeLiterals :: [SugarExpr] -> Context -> ([SugarExpr], Context)
removeLiterals list = runState (do mapM remLiteral list)

remLiteral :: SugarExpr -> SugarState SugarExpr
remLiteral (SugarAff var lit) = do
  lit' <- numToVar lit
  return $ SugarAff var lit'

remLiteral (SugarRep times exprs) = do
  times' <- numToVar times
  exprs' <- mapM remLiteral exprs
  return $ SugarRep times' exprs'

remLiteral (SugarApp proc params res) = do
  params' <- mapM numToVar params
  return $ SugarApp proc params' res

remLiteral (SugarInc var) = do
  return $ SugarInc var

numToVar :: SugLit -> SugarState SugLit
numToVar (SVar v) = return $ SVar v
numToVar (SNum n) = do
  var <- newVar n
  return $ SVar var

newVar :: Integer -> SugarState RepVar
newVar value = do
  ctx <- get
  var <- ctxGFV
  put $ contextSet ctx var value
  return var


ctxGFV :: SugarState Integer
ctxGFV = do
  c <- get
  return $ contextGetFreeVar c

  




