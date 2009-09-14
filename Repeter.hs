import Text.ParserCombinators.Parsec
import Data.Either

import Expr

runExprParser p input
    = case (parse p "" input) of
        Left err ->  RepInc 1313131313 --corriger!!

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

main = interact (show . execute)