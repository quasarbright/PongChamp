module Parser where

import ParseUtils
import AST
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (Alternative(many))
import Text.Megaparsec (choice, sepBy)
import Data.Functor (($>))
import Data.List (foldl')

pExpr :: Parser Expr
pExpr = undefined


pVar :: Parser Expr
pVar = Var <$> identifier -- String -> Expr, Parser String, create Parser Expr

pNum :: Parser Expr
pNum = Number <$> L.decimal

pString :: Parser Expr 
pString = do
    symbol "\""
    String <$> (many L.charLiteral <* symbol "\"")

pBool :: Parser Expr
pBool = choice [ symbol "true" $> Bool True
               , symbol "false" $> Bool False
               ]

pCall :: Parser Expr
pCall = do
    f <- pAtomic
    let pArgs = parens (pExpr `sepBy` symbol ",")
    argss <- many pArgs
    return $ foldl' Call f argss

pAtomic :: Parser Expr
pAtomic = choice [pVar, pBool, pString, pNum, parens pExpr]

-- pUnop = do
--     symbol "!" 
--     return Unop <$> (String <$> op)