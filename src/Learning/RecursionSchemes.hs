{-# LANGUAGE DeriveFunctor #-}
module Learning.RecursionSchemes() where

data ExprF a
    = Var String
    | Number Int
    | Unop a
    | Binop a a
    | Call a [a]
    deriving(Functor)


newtype Fix f = Fix { unFix :: f (Fix f) }
-- newtype Fix f = Fix (f (Fix f))


type Expr = Fix ExprF
-- type Expr = ExprF (Fix ExprF)
-- type Expr = ExprF (ExprF (Fix ExprF))

cata :: (ExprF a -> a) -> Expr -> a
cata f fe_ = let go = cata f in case unFix fe_ of
    Number n -> f (Number n)
    Var s -> f (Var s)
    Unop fe -> f (Unop (go fe))
    Binop fl fr -> f (Binop (go fl) (go fr))
    Call ff fxs -> f (Call (go ff) (go <$> fxs))

endo :: (a -> ExprF a) -> a -> Expr
endo f a = let go = endo f in Fix $ case f a of
    Number n -> Number n
    Var x -> Var x
    Unop a' -> Unop (go a')
    Binop al ar -> Binop (go al) (go ar)
    Call af axs -> Call (go af) (go <$> axs)

buildExpr :: Int -> Expr
buildExpr = endo f where
    f n | n <= 1 = Number 1
    f n = Binop (n - 1) (n - 2)

fibCoalg :: Int -> ExprF Int
fibCoalg n | n <= 1 = Number 1
fibCoalg n = Binop (n - 1) (n - 2)

fibAlg :: ExprF Int -> Int
fibAlg = \case
    Binop l r -> l + r
    Number n -> n
    _ -> 0


fibRed :: Expr -> Int
fibRed = cata $ \case
    Binop l r -> l + r
    Number n -> n
    _ -> 0

fib :: Int -> Int
fib = buildReduce fibCoalg fibAlg

buildReduce :: (a -> ExprF a) -> (ExprF a -> a) -> a -> a
buildReduce coalg alg a = cata alg (endo coalg a)

para :: (ExprF (a, Expr) -> a) -> Expr -> a
para f fe_ = let go fe = (para f fe, fe) in case unFix fe_ of
    Number n -> f (Number n)
    Var x -> f (Var x)
    Unop fe -> f (Unop (go fe))
    Binop fl fr -> f (Binop (go fl) (go fr))
    Call ff fxs -> f (Call (go ff) (go <$> fxs))

data ListF a l = Empty | Cons a l
type List a = Fix (ListF a)

cata' :: (ListF a b -> b) -> List a -> b
cata' f fl_ = let go = cata' f in case unFix fl_ of
    Empty -> f Empty
    Cons x fxs -> f (Cons x (go fxs))

para' :: (ListF a (b, List a) -> b) -> List a -> b
para' f fl_ =
  let go fe = (para' f fe, fe)
   in case unFix fl_ of
        Empty -> f Empty
        Cons x fxs -> f (Cons x (go fxs))
    

-- cata'' :: (ListF a b -> b) -> List a -> b
-- cata'' f = h' (cata'' f)

h' :: (List a -> l) -> (ListF a l -> b) -> List a -> b
h' go f fl_ =
    case unFix fl_ of
        Empty -> f Empty
        Cons x fxs -> f (Cons x (go fxs))



sum' :: List a -> Integer
sum' = para' $ \case
    Empty -> 0
    Cons _ (sumRest, xs) -> 1 + sumRest



cataHelp :: (t1 -> t2 -> t2) -> t2 -> List t1 -> t2
cataHelp combine basecase = cata' $ \case
    Empty -> basecase
    Cons x ansRest -> combine x ansRest


-- vars :: Expr -> Expr
-- vars = \case
--     Number{} -> []
--     Var x -> [x]
--     Unop e -> vars e
--     Binop l r -> vars l ++ vars r
--     Call f xs -> vars f ++ concatMap vars xs

vars :: Expr -> [String]
vars = cata $ \case
    Number{} -> []
    Var x -> [x]
    Unop evars -> evars
    Binop lvars rvars -> lvars ++ rvars
    Call fvars xsvarss -> fvars ++ concat xsvarss

-- newtype Fix f = Fix (f (Fix f))

-- data TF a = Base | Rec a
-- data Maybe a = Nothing | Just a
-- data T = Base | Rec T
-- data Nat = Zero | Succ Nat

-- type Expr = Fix ExprF

-- y f = (\x -> f (x x)) (\x -> f (x x))
-- y f x = (f (f (f (f ...)))) x 

-- y f = f (y f)

-- fac :: (Int -> Int) -> Int -> Int
-- fac recur x = if x == 0 then 1 else x * recur (x - 1)

-- fac (fac (fac (fac ...))) x
