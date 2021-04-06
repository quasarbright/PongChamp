module Interpreter where

import Control.Monad.Except
-- import GameEngine
import Data.Map(Map)
import qualified Data.Map as Map
import AST
import Control.Monad.RWS.Strict
import Control.Arrow
import Data.Functor
import Data.List (sortBy)
import Data.Function
import System.IO
import GameEngine
    ( c_clear, c_delay, c_drawRectangle, c_flip, c_makeEngine )

import qualified Foreign.C.Types as CTypes
import Foreign.Ptr

data Cell =
    CNumber Int
    | CBool Bool
    | CPointer Int
    | CString String
    | CNone
    deriving(Eq, Ord, Show)

-- closed env contains the names which were in scope at the time of the function's creation and their slots. This will include the function itself, for recursion
data Value
    = Closure String Env [String] [Statement]
    | Builtin ([Cell] -> Interpreter Cell)
    | EngineObject (FunPtr ()) -- represents an engine class

instance Show Value where
    show = \case
        Closure f env _ _ -> "<function>: "++f++" "++show env
        Builtin{} -> "<builtin function>"
        EngineObject{} -> "<engine>"

type SymTable = Map String Int -- varname -> stack slot index

type Stack = Map Int Cell -- stack slot index -> Cell
type Heap = Map Int Value -- pointer addr -> Value

type Env = SymTable
type Store = (Stack, Heap, Int, Int)

data RuntimeError
    = UnboundVar String
    | BadDeref
    | TypeError String
    | ArityError String
    | UserError Cell
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


builtinPrint_ :: [Cell] -> Interpreter Cell
builtinPrint_ cs = mapM printSingle cs $> CNone
    where
        printSingle = \case
            CPointer addr -> do
                v <- deref addr
                liftIO (putStr (show v) >> hFlush stdout)
            c -> liftIO (putStr (show c) >> hFlush stdout)

builtinPrint :: Value
builtinPrint = Builtin builtinPrint_

builtinPrintln :: Value
builtinPrintln = Builtin $ \cs -> builtinPrint_ cs >> liftIO (putStrLn "") $> CNone

sortMap :: Ord a1 => ((k, a2) -> a1) -> Map k a2 -> [(k, a2)]
sortMap key m = m & Map.toList & sortBy (compare `on` key)

printState :: Value
printState = Builtin $ \_ -> do
    stack <- getStack
    heap <- getHeap
    env <- ask
    liftIO (do
        putStrLn "symtable:"
        print (sortMap snd env)
        putStrLn "stack:"
        print (sortMap fst stack)
        putStrLn "heap:"
        print (sortMap fst heap))
    return CNone

input :: Value
input = Builtin $ const $ CString <$> liftIO getLine

str :: Value
str = Builtin $ \case
    [c] -> return (CString (show c)) -- TODO pretty
    _ -> throwError (ArityError "str")
square :: CTypes.CInt -> CTypes.CInt
square x = x * x

toCInt :: Int -> CTypes.CInt
toCInt = fromIntegral

builtinGameEngine :: Value
builtinGameEngine = Builtin $ \cs -> do
    engine <- case cs of
        [c1, c2] -> case (c1, c2) of
            (CNumber w, CNumber h) ->
                liftIO(do
                        print "making engine with: " >> print w >> print ", " >> print h
                        c_makeEngine (toCInt w) (toCInt h))
            _ -> throwError (TypeError "game engine needs int width and height")
        _ -> throwError (ArityError "game engine needs width and height")
    let eng = EngineObject engine
    malloc' eng

-- Begin Engine API --
getEngineObjectC :: Cell -> Interpreter (FunPtr ())
getEngineObjectC c = case c of
    CPointer i -> do
        deref i >>= \case
            EngineObject eng -> return eng
            _ -> throwError (TypeError "engine function needs an engine object")
    _ -> throwError (TypeError "engine function needs an engine object")

getEngineObject :: [Cell] -> Interpreter (FunPtr ())
getEngineObject cs = case cs of
    cl:_ -> do
        getEngineObjectC cl
    _ -> throwError (ArityError "engine function needs game engine object")

engineClear :: Value
engineClear = Builtin $ \cs -> do
    eng <- getEngineObject cs
    liftIO(do
            c_clear eng)
    return CNone

engineFlip :: Value
engineFlip = Builtin $ \cs -> do
    eng <- getEngineObject cs
    liftIO (do
            c_flip eng
        )
    return CNone

engineDelay :: Value
engineDelay = Builtin $ \(c:cs)-> do
    case cs of
        [c1] -> do
            eng <- getEngineObjectC c
            case c1 of
                (CNumber delay) ->
                    liftIO(do
                            c_delay eng (toCInt delay)
                        )
                _ -> throwError (TypeError "delay expects one int")
        _ -> do
            throwError (ArityError "need 1 arg")
    return CNone

engineDrawRect :: Value
engineDrawRect = Builtin $ \(c:cs) -> do
    case cs of
        [c1, c2, c3, c4] -> do
            eng <- getEngineObjectC c
            case (c1, c2, c3, c4) of
                (CNumber x, CNumber y, CNumber w, CNumber h) ->
                    liftIO(do
                        c_drawRectangle eng (toCInt x) (toCInt y) (toCInt w) (toCInt h)
                    )
                _ -> throwError (TypeError "draw rect expects four ints")
        _ -> throwError (ArityError "need 5 args")
    return CNone

-- End Engine API --

stdLib :: [(String, Value)]
stdLib =
    [ ("println", builtinPrintln)
    , ("print", builtinPrint)
    , ("__printState__", printState)
    , ("input", input)
    , ("str", str)
    , ("callEngine", builtinGameEngine)
    , ("clearEngine", engineClear)
    , ("flipEngine", engineFlip)
    , ("delayEngine", engineDelay)
    , ("drawRect", engineDrawRect)
    ]

initialize :: Interpreter Env
initialize = do
    let help name value = do
            ptr <- malloc' value
            si <- salloc ptr
            return (name,si)
    env0 <- mapM (uncurry help) stdLib
    return $ Map.fromList env0

getStack :: Interpreter Stack
getStack = gets (\(s,_,_,_) -> s)

getsStack :: (Stack -> a) -> Interpreter a
getsStack f = f <$> getStack

putStack :: Stack -> Interpreter ()
putStack = modifyStack . const

modifyStack :: (Stack -> Stack) -> Interpreter ()
modifyStack f = modify $ \(s,a,b,c) -> (f s,a,b,c)

getHeap :: Interpreter Heap
getHeap = gets (\(_,h,_,_) -> h)

getsHeap :: (Heap -> a) -> Interpreter a
getsHeap f = f <$> getHeap

putHeap :: Heap -> Interpreter ()
putHeap = modifyHeap . const

modifyHeap :: (Heap -> Heap) -> Interpreter ()
modifyHeap f = modify $ \(a,h,b,c) -> (a,f h,b,c)


-- | new heap address. increments next available address
newAddr :: Interpreter Int
newAddr = do
    (s,h,si,hi) <- get
    put (s,h,si,succ hi)
    return hi

-- | newAddr but for the stack. Gets the next stack slot and increments next available
newSlot :: Interpreter Int
newSlot = do
    (s,h,si,hi) <- get
    put (s,h,succ si,hi)
    return si

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

declareVar :: String -> Interpreter Result -> Interpreter Result
declareVar s mRest = do
    si <- newSlot
    withVar s si mRest

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
                    Closure _ env argnames body -> do
                        unless (length cargs == length argnames) (throwError (ArityError "called with bad number of args"))
                        sis <- mapM salloc cargs
                        let argBindings = zip argnames sis
                            argBindings' = Map.fromList argBindings
                            env' = Map.union argBindings' env
                        usingEnv env' (runFunctionBody body)
                    Builtin func -> func cargs
                    EngineObject _ -> throwError (TypeError "engine object is not callable")
            _ -> throwError (TypeError "applied non-function")

runFunctionBody :: [Statement] -> Interpreter Cell
runFunctionBody body = runStatements body >>= \case
    Returned c -> return c
    Broke -> return CNone
    Continued -> return CNone
    Normal -> return CNone

(>>!) :: Interpreter Result -> Interpreter Result -> Interpreter Result
mr >>! ma = do
    mr >>= \case
        Normal -> ma
        r -> return r

runStatements :: [Statement] -> Interpreter Result
runStatements [] = return Normal
runStatements (s_:rest) =
    let mRest = runStatements rest
    in case s_ of
        Let x Nothing -> declareVar x mRest
        Let x (Just a) -> let rest' = (Let x Nothing:Assign x a:rest) in runStatements rest'
        Assign x e -> (assignVar x =<< evalExpr e) >> mRest
        Eval e -> evalExpr e >> mRest
        Break -> return Broke
        Continue -> return Continued
        Return e -> Returned <$> evalExpr e
        If cnd thn mEls -> do
            evalExpr cnd >>= \case
                CBool True -> runStatements thn >>! mRest
                CBool False -> maybe (return Normal) runStatements mEls >>! mRest
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
            fsi <- salloc CNone
            let fVars = freeVars s_
                env' = Map.filterWithKey (\k _ -> k `elem` fVars) env
                env'' = Map.insert f fsi env' -- include self in env for recursion
                closure = Closure f env'' argnames body
            fptr <- malloc' closure
            modifyStack (Map.insert fsi fptr) -- then update the stack with the pointer to the closure in the heap
            withVar f fsi mRest
        Throw e -> do
            c <- evalExpr e
            throwError (UserError c)
        TryCatch tryStmts x catchStmts -> do
            let mtc = catchError (runStatements tryStmts) $ \case
                        UserError err -> declareVar x (assignVar x err >> runStatements catchStmts)
                        err -> throwError err
            mtc >>! mRest
            


runProgram :: Program -> Interpreter Result
runProgram (Program stmts) = runStatements stmts

interpretProgram :: Program -> IO (Either RuntimeError Result)
interpretProgram = evalInterpreter . runProgram

evalInterpreter :: Interpreter a -> IO (Either RuntimeError a)
evalInterpreter m = mio
    where
        m' = do
            env0 <- initialize
            local (const env0) m
        mrws = runInterpreter m'
        me = fst <$> evalRWST mrws mempty (mempty, mempty, 0,0)
        mio = runExceptT me
