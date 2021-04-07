{-# LANGUAGE FlexibleInstances #-}
module AST where

import Data.List(union)

newtype Program = Program [Statement] deriving(Eq, Ord, Show)

data Statement
    = While Expr [Statement]
    | If Expr [Statement] (Maybe [Statement])
    | Let String (Maybe Expr)
    | Assign String Expr
    | Eval Expr
    | Function String [String] [Statement]
    | Return Expr
    | Break
    | Continue
    | Throw Expr
    | TryCatch [Statement] String [Statement]
    deriving(Eq, Ord, Show)

data Binop = Plus | Minus | Times | FloorDiv | Or | And | Eq | Neq | Lt | Le | Gt | Ge deriving(Eq, Ord, Show)
data Unop = Not deriving(Eq, Ord, Show)

data Expr
    = Var String
    | Number Int
    | String String
    | Bool Bool
    | Unop Unop Expr
    | Binop Binop Expr Expr
    | Call Expr [Expr]
    | ObjectLiteral [(String, Expr)]
    | FieldAccess Expr String
    deriving(Eq, Ord, Show)

class FreeVars e where
    freeVars :: e -> [String]

instance FreeVars Expr where
    freeVars = \case
        Var x -> [x]
        Number{} -> mempty
        String{} -> mempty
        Bool{} -> mempty
        Unop _ e -> freeVars e
        Binop _ l r -> freeVars l <>. freeVars r
        Call f xs -> freeVars (f:xs)
        ObjectLiteral props -> freeVars (snd <$> props)
        FieldAccess e _ -> freeVars e

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

(<>.) :: Eq a => [a] -> [a] -> [a]
(<>.) = union

withouts :: Eq a => [a] -> [a] -> [a]
withouts xs = filter (`notElem` xs)

instance FreeVars [Expr] where
    freeVars xs = unions (freeVars <$> xs)

instance FreeVars a => FreeVars (Maybe a) where
    freeVars (Just a) = freeVars a
    freeVars Nothing = []

newtype Block = Block{unBlock :: [Statement]}

instance FreeVars [Statement] where
    freeVars stmts = foldr go [] stmts
        where
            -- | immediate vars bound (doesn't recur)
            boundVars = \case
                Let x _ -> [x]
                Function f args _ -> [f] <>. args 
                Assign{} -> []
                While{} -> []
                If{} -> []
                Eval{} -> []
                Return{} -> []
                Break{} -> []
                Continue{} -> []
                Throw{} -> []
                TryCatch{} -> []
            go stmt rest = freeVars stmt <>. withouts (boundVars stmt) rest

instance FreeVars Statement where
    freeVars = \case
        While e body -> freeVars e <>. freeVars body
        If e thn mEls -> freeVars e <>. freeVars thn <>. freeVars mEls
        Let _ mRhs -> freeVars mRhs
        Assign x rhs -> [x] <>. freeVars rhs
        Eval e -> freeVars e
        Function f args body -> withouts (f:args) (freeVars body)
        Return e -> freeVars e
        Break -> []
        Continue -> []
        Throw e -> freeVars e
        TryCatch tryStmts x catchStmts -> freeVars tryStmts <>. withouts [x] (freeVars catchStmts)
