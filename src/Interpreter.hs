module Interpreter where

import Control.Monad.Except
-- import GameEngine
import Data.Map(Map)
import qualified Data.Map as Map
import AST
import Control.Monad.RWS.Strict
import Control.Arrow

data Cell = 
    CNumber Int
    | CBool Bool
    | CPointer Int
    | CString String
    | CNone
    deriving(Eq, Ord, Show)

data Value = Closure Env [String] [Statement]
-- CFunction FunPtr


type SymTable = Map String Int -- varname -> stack slot index

type Stack = Map Int Cell -- stack slot index -> Cell
type Heap = Map Int Value -- pointer addr -> Value

type Env = SymTable
type Store = (Stack, Heap)

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

getStack :: Interpreter Stack
getStack = gets fst

getsStack :: (Stack -> a) -> Interpreter a
getsStack f = gets (f . fst)

putStack :: Stack -> Interpreter ()
putStack = modifyStack . const

modifyStack :: (Stack -> Stack) -> Interpreter ()
modifyStack = modify . first

getHeap :: Interpreter Heap
getHeap = gets snd

getsHeap :: (Heap -> a) -> Interpreter a
getsHeap f = gets (f . snd)

putHeap :: Heap -> Interpreter ()
putHeap = modifyHeap . const

modifyHeap :: (Heap -> Heap) -> Interpreter ()
modifyHeap = modify . second


-- | new heap address. increments next available address
newAddr :: Interpreter Int
newAddr = do
    heap <- getHeap
    let keys = Map.keys heap
    let maxKey = 
            case keys of
                [] -> 0
                _ -> maximum keys
    return (succ maxKey)

-- | newAddr but for the stack. Gets the next stack slot and increments next available
newSlot :: Interpreter Int
newSlot = do
    stack <- getStack
    let keys = Map.keys stack
    let maxKey = 
            case keys of
                [] -> 0
                _ -> maximum keys
    return (succ maxKey)

-- | put value on the heap and return the address of it
malloc :: Value -> Interpreter Int
malloc v = do
    addr <- newAddr
    modifyHeap (Map.insert addr v)
    return addr

-- | put cell on the stack and return stack slot of it
salloc :: Cell -> Interpreter Int
salloc c = do
    si <- newSlot
    modifyStack (Map.insert si c)
    return si

-- | put value on the heap and return a pointer to it
malloc' :: Value -> Interpreter Cell
malloc' v = CPointer <$> malloc v

-- | get the value at addr on the heap
deref :: Int -> Interpreter Value
deref addr = do
    heap <- getHeap
    case Map.lookup addr heap of
        Nothing -> throwError BadDeref
        Just v -> return v


withVar :: String -> Cell -> Interpreter a -> Interpreter a
withVar x c m = do
    si <- salloc c
    local (Map.insert x si) m

withVars :: [(String, Cell)] -> Interpreter a -> Interpreter a
withVars pairs m = do
    let (vars, cells) = unzip pairs
    sis <- mapM salloc cells
    let stack' = zip vars sis
    local (Map.union (Map.fromList stack')) m

usingEnv :: Env -> Interpreter a -> Interpreter a
usingEnv e = local (const e)

stackLookup :: Int -> Interpreter Cell
stackLookup i = do
    stack <- getStack
    case Map.lookup i stack of
        Nothing -> throwError BadDeref
        Just c -> return c

symLookup :: String -> Interpreter Cell
symLookup x = do
    env <- ask
    case Map.lookup x env of
        Nothing -> throwError (UnboundVar x)
        Just slot -> stackLookup slot

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
runStatements = undefined 
-- runStatements [] = return Normal
-- runStatements (s_:rest) =
--     let mRest = runStatements rest
--     in case s_ of
--         Let x None -> 
        -- While cnd body -> do
        --     evalExpr cnd >>= \case
        --         CBool True -> do
        --             rbody <- runStatements body


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

-- State (Map String Cell)

-- top.x = 2
-- function top.f() {
--     let top.f.x = top.x + 1
--     return top.f.x
-- }
-- x = 30
-- z = 3
-- y = f()

runProgram :: Program -> Interpreter ()
runProgram = undefined

evalInterpreter :: Interpreter a -> IO (Either RuntimeError a)
evalInterpreter m = mio 
    where
        mrws = runInterpreter m
        me = fst <$> evalRWST mrws mempty mempty
        mio = runExceptT me

