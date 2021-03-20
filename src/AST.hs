module AST where

newtype Program = Program [Statement] deriving(Eq, Ord, Show)

data Statement
    = While Expr [Statement]
    | If Expr [Statement] (Maybe [Statement])
    | Assign String Expr
    | Eval Expr
    | Function [String] [Statement]
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
    deriving(Eq, Ord, Show)


