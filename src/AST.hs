{-# LANGUAGE FlexibleInstances #-}
module AST where

import Data.List(union)

newtype Program = Program [Statement] deriving(Eq, Ord, Show)

data LHS
    = LVar String -- x
    | LField Expr String -- (e).x
    | LIndex Expr Expr -- e[e]
    deriving(Eq, Ord, Show)

type RHS = Expr -- @ ryan

data Statement
    = While Expr [Statement]
    | For Statement Expr Statement [Statement] -- for (let x = 1; x < 10; x = x + 1) { print(x); }
    | Foreach String Expr [Statement] -- for (let x of [1,2,3]) { print(x); }
    | If Expr [Statement] (Maybe [Statement])
    | Let String (Maybe Expr)
    | Assign LHS Expr
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
    | Unop Unop Expr -- !e
    | Binop Binop Expr Expr -- e + e
    | Call Expr [Expr] -- e(e,e)
    | ObjectLiteral [(String, Expr)] -- {x: e, x: e}
    | ArrayLiteral [Expr] -- [e,e,e]
    | FieldAccess Expr String -- e.x
    | IndexAccess Expr Expr -- e[e]
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
        Binop _ l r -> freeVars [l,r]
        Call f xs -> freeVars (f:xs)
        ObjectLiteral props -> freeVars (snd <$> props)
        ArrayLiteral es -> freeVars es
        FieldAccess e _ -> freeVars e
        IndexAccess e ind -> freeVars [e,ind]

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
                For{} -> []
                Foreach{} -> []
                If{} -> []
                Eval{} -> []
                Return{} -> []
                Break{} -> []
                Continue{} -> []
                Throw{} -> []
                TryCatch{} -> []
            go stmt rest = freeVars stmt <>. withouts (boundVars stmt) rest

instance FreeVars LHS where
    freeVars = \case
        LVar x -> [x]
        LField obj _ -> freeVars obj
        LIndex e ind -> freeVars [e,ind]

-- for(let x; x && y; ;) { print(y); print(x); }

forToWhile :: Statement -> Expr -> Statement -> [Statement] -> [Statement]
forToWhile initialize condition update body = [initialize, While condition (body ++ [update])]

-- what variables are free in this particular statement
instance FreeVars Statement where
    freeVars = \case
        While e body -> freeVars e <>. freeVars body
        For initialize condition update body -> freeVars (forToWhile initialize condition update body)
        Foreach x iterable body -> freeVars iterable <>. withouts [x] (freeVars body)
        If e thn mEls -> freeVars e <>. freeVars thn <>. freeVars mEls
        Let _ mRhs -> freeVars mRhs
        Assign lhs rhs -> freeVars lhs <>. freeVars rhs
        Eval e -> freeVars e
        Function f args body -> withouts (f:args) (freeVars body)
        Return e -> freeVars e
        Break -> []
        Continue -> []
        Throw e -> freeVars e
        TryCatch tryStmts x catchStmts -> freeVars tryStmts <>. withouts [x] (freeVars catchStmts)
