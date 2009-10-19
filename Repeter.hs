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

import Text.ParserCombinators.Parsec
import Data.Either

import Expr


runExprParser p input
    = case (parse (many p) "" input) of
        Left err -> [RepInc 1313131313] --corriger!!

        Right x  -> x

parseVar :: Parser RepVar
parseVar = do { oneOf "rR"
              ; number <- many1 digit
              ; return $ read number
              }


parseInc :: Parser RepExpr
parseInc = do { string "inc"
              ; spaces ; char '(' ; spaces
              ; var <- parseVar
              ; spaces
              ; char ')'
              ; spaces
              ; return $ RepInc var
              }
             
parseAff :: Parser RepExpr
parseAff = do { f <- parseVar
              ; spaces
              ; string "<-"
              ; spaces
              ; s <- parseVar
              ; spaces
              ; return $ RepAff f s
              }

parseRep :: Parser RepExpr
parseRep = do { string "repeter"
              ; spaces 
              ; times <- parseVar
              ; spaces
              ; string "fois"
              ; spaces ; char '[' ; spaces
              ; body <- many parseExpr
              ; spaces ; char ']' ; spaces
              ; return $ RepRep times body
              }

parseExpr :: Parser RepExpr
parseExpr = try parseInc <|> try parseAff <|> parseRep

readIntegerList :: String -> [Integer]
readIntegerList l = read l

execute prog = contextResult (evaluer ctx expr)
    where
      ctx = (contextFromList . readIntegerList . head .lines) prog
      expr = (runExprParser parseExpr ((concat . tail . lines) prog))

parseOnly prog = (runExprParser parseExpr ((concat . tail . lines) prog))

main = interact (show . execute)

--main = interact (show . parseOnly)