module Parser where

import ParseUtils
import AST
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Control.Applicative ( Alternative(many) )
import Text.Megaparsec hiding (many)
import Data.Functor (($>))
import Data.List (foldl')
import Text.Megaparsec.Char (char)
import Data.Void

-- expr --

pExpr :: Parser Expr
pExpr = pBinop

binary :: String -> Binop -> Operator Parser Expr
binary name op = InfixL $ do
    symbol name
    return $ \l r -> Binop op l r

pBinop :: Parser Expr
pBinop = makeExprParser pUnop table
    where
        table =
            [ [binary "*" Times, binary "/" FloorDiv]
            , [binary "+" Plus, binary "-" Minus]
            , [binary "<" Lt, binary ">" Gt, binary "<=" Le, binary ">=" Ge]
            , [binary "==" Eq, binary "!=" Neq]
            , [binary "&&" And, binary "||" Or]
            ]

prefix :: String -> Unop -> Operator Parser Expr
prefix  name op = Prefix $ do
    symbol name
    return $ \e -> Unop op e

pUnop :: Parser Expr
pUnop = makeExprParser pCall table
    where
        table = [[prefix "!" Not]]

pCall :: Parser Expr
pCall = do
    f <- pAtomic
    let pArgs = parens (pExpr `sepBy` symbol ",")
    argss <- many pArgs
    return $ foldl' Call f argss

pAtomic :: Parser Expr
pAtomic = choice [pVar, pBool, pString, pNum, parens pExpr]

pVar :: Parser Expr
pVar = Var <$> identifier -- String -> Expr, Parser String, create Parser Expr

pNum :: Parser Expr
pNum = Number <$> lexeme L.decimal

pString :: Parser Expr
pString = String <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pBool :: Parser Expr
pBool = choice [ symbol "true" $> Bool True
               , symbol "false" $> Bool False
               ]

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = runParser pExpr ""

-- statements --

pStatement :: Parser Statement
pStatement = choice [pWhile, pIf, pFunction, try pAssign, pEval]

pBlock :: Parser [Statement]
pBlock = braces (many pStatement) <|> fmap (:[]) pStatement

pWhile :: Parser Statement
pWhile = do
    pKeyword "while"
    cond <- parens pExpr
    body <- pBlock
    return $ While cond body

pIf :: Parser Statement
pIf = do
    pKeyword "if"
    cond <- parens pExpr
    thn <- pBlock
    mEls <- optional (pKeyword "else" *> pBlock)
    return $ If cond thn mEls


pFunction :: Parser Statement
pFunction = do
    pKeyword "function"
    f <- identifier
    args <- parens (identifier `sepBy` symbol ",")
    body <- pBlock
    return $ Function f args body

pAssign :: Parser Statement
pAssign = do
    x <- identifier
    symbol "="
    rhs <- pExpr
    symbol ";"
    return $ Assign x rhs

pEval :: Parser Statement
pEval = Eval <$> pExpr <* symbol ";"

parseStatement :: String -> Either (ParseErrorBundle String Void) Statement
parseStatement = runParser pStatement ""

-- program --

pProgram :: Parser Program
pProgram  = Program <$> many pStatement

-- | name then source
parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Program 
parseProgram = runParser pProgram
