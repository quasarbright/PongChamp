module Learning.Parser2ElectricBoogaloo where

import Control.Applicative ( Applicative(liftA2) )
import Control.Monad ( void )

data Expr = Num Int | Plus Expr Expr deriving (Show)
newtype Parser a = Parser {runParser :: String -> Either String (String, a)}

instance Functor Parser where
    fmap a2b pa = Parser $ \s -> case runParser pa s of
        Left err -> Left err
        Right (s', a) -> Right (s', a2b a)

instance Applicative Parser where
    pure a = Parser $ \s -> Right (s, a)
    liftA2 ab2c pa pb = Parser $ \s -> case runParser pa s of
        Left err -> Left err
        Right (s', a) -> case runParser pb s' of
            Left err -> Left err
            Right (s'', b) -> Right (s'', ab2c a b)

instance Monad Parser where
    return = pure
    pa >>= a2pb = Parser $ \s -> case runParser pa s of
        Left err -> Left err
        Right (s', a) -> runParser (a2pb a) s'

satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate = Parser $ \case
    "" -> Left "Expected char"
    c:s
        | predicate c -> return (s, c)
        | otherwise -> Left "Unexpected char"

char :: Char -> Parser ()
char c = void $ satisfies (==c)

voidParser :: Parser ()
voidParser = do
    return ()

oneOf :: [Char] -> Parser Char
oneOf chars = satisfies (`elem` chars)

digit :: Parser Char
digit = oneOf "0123456789"

nat:: Parser Int
nat = read <$> some digit

optional:: Parser a -> Parser (Maybe a)
optional pa = Parser $ \s -> case runParser pa s of
    Left _ -> return (s, Nothing)
    Right (s', a) -> return (s', Just a)

throw :: String -> Parser a
throw err = Parser $ \_ -> Left err

many :: Parser a -> Parser [a]
many pa = do
    ma <- optional pa
    case ma of
        Nothing -> return []
        Just a -> do
            rest <- many pa
            return (a:rest)

some :: Parser a -> Parser [a]
some pa = do
    as <- many pa
    case as of
        [] -> throw "Need at least one thing"
        _ -> return as


pnum :: Parser Expr
pnum = Num <$> nat

pplus :: Parser Expr
pplus = do
    num1 <- pnum
    mu <- optional $ char '+'
    case mu of
        Nothing -> return num1
        Just () -> Plus num1 <$> pplus

-- many pa = Parser $ \s -> case runParser pa s of
--     Left _ -> return (s, [])
--     Right (s', a) -> do
--         (s'', rest) <- runParser pa s'
--         return a:rest
