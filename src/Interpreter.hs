module Interpreter where

import Control.Monad.Except
import Control.Monad.RWS.Strict
-- import GameEngine
import Data.Map(Map)
import qualified Data.Map as Map
import AST

type Env = Map String Cell

data Cell = 
    CNumber Int
    | CBool Bool
    | CPointer Int
    | CString String
    | CNone
    deriving(Eq, Ord, Show)

data Value = Closure Env [String] [Statement]
-- CFunction FunPtr

type Store = Map Int Value
data RuntimeError
    = UnboundVar String
    | BadDeref
    | TypeError String
    | ArityError String

data Result
    = Returned Cell
    | Broke
    | Continued
    -- TODO errors
    | Normal
    deriving(Eq, Ord, Show)



newtype Interpreter a = Interpreter
    { runInterpreter :: (RWST Env () Store (ExceptT RuntimeError IO)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError RuntimeError
        , MonadState Store
        , MonadReader Env
        , MonadIO
        )

newAddr :: Interpreter Int
newAddr = do
    heap <- get
    let keys = Map.keys heap
    let maxKey = 
            case keys of
                [] -> 0
                _ -> maximum keys
    return (succ maxKey)

malloc :: Value -> Interpreter Int
malloc v = do
    addr <- newAddr
    modify (Map.insert addr v)
    return addr

malloc' :: Value -> Interpreter Cell
malloc' v = CPointer <$> malloc v

deref :: Int -> Interpreter Value
deref addr = do
    heap <- get
    case Map.lookup addr heap of
        Nothing -> throwError BadDeref
        Just v -> return v

withVar :: String -> Cell -> Interpreter a -> Interpreter a
withVar x c = local (Map.insert x c)

withVars :: [(String, Cell)] -> Interpreter a -> Interpreter a
withVars vars = local (Map.union (Map.fromList vars))

withVars' :: Map String Cell -> Interpreter a -> Interpreter a
withVars' vars = local (Map.union vars)

usingEnv :: Env -> Interpreter a -> Interpreter a
usingEnv e = local (const e)

stackLookup :: String -> Interpreter Cell
stackLookup x = do
    stack <- ask
    case Map.lookup x stack of
        Nothing -> throwError (UnboundVar x)
        Just c -> return c

wrapArith :: (Int -> Int -> Int) -> Cell -> Cell -> Interpreter Cell
wrapArith op l r = case (l,r) of
    (CNumber a, CNumber b) -> return (CNumber (op a b))
    _ -> throwError (TypeError "arithmetic expected numbers")

wrapCmp :: (Int -> Int -> Bool) -> Cell -> Cell -> Interpreter Cell
wrapCmp op l r = case (l, r) of
    (CNumber a, CNumber b) -> return (CBool (op a b))
    _ -> throwError (TypeError "comparison expected numbers")

evalExpr :: Expr -> Interpreter Cell
evalExpr = \case
    Var x -> stackLookup x
    Number n -> return (CNumber n)
    Bool b -> return (CBool b)
    String s -> return (CString s)
    Unop Not e -> do
        c <- evalExpr e
        case c of
            CBool b -> return (CBool (not b))
            _ -> throwError (TypeError "not expects bool")
    Binop And l r -> do
        cL <- evalExpr l
        case cL of
            CBool False -> return (CBool False)
            CBool True -> do
                cR <- evalExpr r
                case cR of
                    CBool b -> return (CBool b)
                    _ -> throwError (TypeError "and expected bool")
            _ -> throwError (TypeError "and expected bool")
    Binop Or l r -> do
        cL <- evalExpr l
        case cL of
            CBool True  -> return (CBool True)
            CBool False -> do
                cR <- evalExpr r
                case cR of
                    CBool b -> return (CBool b)
                    _ -> throwError (TypeError "or expected bool")
            _ -> throwError (TypeError "or expected bool")
    Binop b l r -> do
        cL <- evalExpr l
        cR <- evalExpr r
        case b of
            Plus -> wrapArith (+) cL cR
            Minus -> wrapArith (-) cL cR
            Times -> wrapArith (*) cL cR
            FloorDiv -> wrapArith div cL cR
            Eq -> return (CBool (cL == cR))
            Neq -> return (CBool (cL /= cR))
            Lt -> wrapCmp (<) cL cR
            Gt -> wrapCmp (>) cL cR
            Le -> wrapCmp (<=) cL cR
            Ge -> wrapCmp (>=) cL cR
            Or -> error "impossible"
            And -> error "impossible"
    Call f args -> do
        cf <- evalExpr f
        cargs <- mapM evalExpr args
        case cf of
            CPointer addr -> do
                deref addr >>= \case
                    Closure env argnames body -> do
                        unless (length cargs == length argnames) (throwError (ArityError "called with bad number of args"))
                        let argBindings = zip argnames cargs
                            argBindings' = Map.fromList argBindings
                            env' = Map.union argBindings' env
                        usingEnv env' (runFunctionBody body) -- TODO update with result shit from brack 
            _ -> throwError (TypeError "applied non-function")

runFunctionBody :: [Statement] -> Interpreter Cell
runFunctionBody body = runStatements body >>= \case
    Returned c -> return c
    Broke -> return CNone
    Continued -> return CNone
    Normal -> return CNone

runStatements :: [Statement] -> Interpreter Result
runStatements [] = return Normal
runStatements (s_:rest) =
    let mRest = runStatements rest
    in case s_ of
        While cnd body -> do
            evalExpr cnd >>= \case
                CBool True -> do
                    rbody <- runStatements body


{-
CRISIS

x = 2
function f() {
    return x
}
x = 30
z = 3
y = f()

function g() {
    let x = 234234
    return f()
}





we don't want f to see z, we want f to think x is 30 so it needs that mutation
but you can't just use dynamic scope naively

let top.x = 2
function top.f() {
    let top.f.x = top.x + 1
    return top.f.x
}
top.x = 30
let top.z = 3
let top.y = top.f()

solution: tag everything to uniquify names and then use dynamic scope
also you need to add let to declare

-}

State (Map String Cell)

top.x = 2
function top.f() {
    let top.f.x = top.x + 1
    return top.f.x
}
x = 30
z = 3
y = f()

runProgram :: Program -> Interpreter ()
runProgram = undefined

evalInterpreter :: Interpreter a -> IO (Either RuntimeError a)
evalInterpreter m = mio 
    where
        mrws = runInterpreter m
        me = fst <$> evalRWST mrws mempty mempty
        mio = runExceptT me

