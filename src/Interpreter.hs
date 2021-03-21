module Interpreter where

import Control.Monad.Except
-- import GameEngine
import Data.Map(Map)
import qualified Data.Map as Map
import AST
import Control.Monad.RWS.Strict
import Control.Arrow
import Data.Functor

data Cell = 
    CNumber Int
    | CBool Bool
    | CPointer Int
    | CString String
    | CNone
    deriving(Eq, Ord, Show)

data Value = Closure Env [String] [Statement] deriving(Eq, Ord, Show)
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
    deriving(Eq, Ord, Show)

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


withVar :: String -> Int -> Interpreter a -> Interpreter a
withVar x si = local (Map.insert x si)

assignVar :: String -> Cell -> Interpreter ()
assignVar x c = do
    si <- symLookup_ x
    modifyStack (Map.insert si c)

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

-- Gives the stack slot of the variable
symLookup_ :: String -> Interpreter Int
symLookup_ x = do
    env <- ask
    case Map.lookup x env of
        Nothing -> throwError (UnboundVar x)
        Just slot -> return slot

-- Gives the cell of the variable (Reads stack slot)
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
    Var x -> symLookup x
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
                    -- env includes the globals that f can use
                    Closure env argnames body -> do
                        unless (length cargs == length argnames) (throwError (ArityError "called with bad number of args")) 
                        sis <- mapM salloc cargs
                        let argBindings = zip argnames sis
                            argBindings' = Map.fromList argBindings
                            env' = Map.union argBindings' env
                        usingEnv env' (runFunctionBody body)
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
        Let x Nothing -> do
            si <- newSlot
            withVar x si mRest
        Let x (Just a) -> let rest' = (Let x Nothing:Assign x a:rest) in runStatements rest'
        Assign x e -> (assignVar x =<< evalExpr e) >> mRest
        Eval e -> evalExpr e >> mRest
        Break -> return Broke
        Continue -> return Continued
        Return e -> Returned <$> evalExpr e
        If cnd thn mEls -> do
            evalExpr cnd >>= \case
                CBool True -> runStatements thn >> mRest
                CBool False -> maybe (return Normal) runStatements mEls >> mRest
                _ -> throwError (TypeError "if expected bool")            
        While cnd body -> do
            evalExpr cnd >>= \case
                CBool True -> do
                    let again = let rest' = (While cnd body:rest) in runStatements rest' -- run loop again
                    runStatements body >>= \case
                        Returned c -> return $ Returned c
                        Broke -> mRest
                        Continued -> again
                        Normal -> again
                CBool False  -> mRest
                _ -> throwError (TypeError "while expected bool")
        Function f argnames body -> do
            env <- ask -- symtable
            fsi <- newSlot
            let env' = Map.insert f fsi env -- include self in env for recursion
                closure = Closure env' argnames body
            fptr <- malloc' closure
            modifyStack (Map.insert fsi fptr) -- then update the stack with the pointer to the closure in the heap
            withVar f fsi mRest
        -- rbody <- runStatements body


runProgram :: Program -> Interpreter Result
runProgram (Program stmts) = runStatements stmts

interpretProgram :: Program -> IO (Either RuntimeError Result)
interpretProgram = evalInterpreter . runProgram

evalInterpreter :: Interpreter a -> IO (Either RuntimeError a)
evalInterpreter m = mio 
    where
        mrws = runInterpreter m
        me = fst <$> evalRWST mrws mempty mempty
        mio = runExceptT me
