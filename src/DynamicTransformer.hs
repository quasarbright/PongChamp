module DynamicTransformer where

import Control.Monad.Except
import Control.Monad.RWS.Strict
-- import GameEngine
import Data.Map(Map)
import qualified Data.Map as Map
import AST
import Control.Monad.Identity

type Env = Map String String -- map of old variable names to new

type Store = Int

data TransformerError
    = UnboundVar String
    | BadDeref
    | TypeError String
    | ArityError String
    deriving(Show)

newtype Transformer a = Transformer
    {
        runTransformer :: (RWST Env () Store (ExceptT TransformerError Identity)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError TransformerError
        , MonadState Store
        , MonadReader Env
        )

newVar :: Transformer String
newVar = do
    nextVar <- get
    modify succ
    return $ "$" ++ show nextVar

envLookup :: String -> Transformer String
envLookup x = do
    env <- ask
    case Map.lookup x env of
        Nothing -> throwError (UnboundVar x)
        Just c -> return c

withVar :: String -> String -> Transformer a -> Transformer a
withVar x x' = local (Map.insert x x')

withVars :: [(String, String)] -> Transformer a -> Transformer a
withVars vars = local (Map.union (Map.fromList vars))

withVars' :: Map String String -> Transformer a -> Transformer a
withVars' vars = local (Map.union vars)

usingEnv :: Env -> Transformer a -> Transformer a
usingEnv e = local (const e)

transformExpr :: Expr -> Transformer Expr
transformExpr = \case
    Var x -> Var <$> envLookup x
    e@Number{} -> return e
    e@String{} -> return e
    e@Bool{} -> return e
    Unop op u -> Unop op <$> transformExpr u
    Binop op l r -> Binop op <$> transformExpr l <*> transformExpr r
    Call name vars -> do
        cname <- transformExpr name
        cargs <- mapM transformExpr vars
        return $ Call cname cargs

transformStatements :: [Statement] -> Transformer [Statement]
transformStatements [] = return []
transformStatements (s_:rest) =
    let mRest = transformStatements rest
        appendRest = appendRestWith id
        appendRestWith f s = do
            rest' <- f mRest
            return (s:rest')
    in case s_ of
        While e stmts -> do
            e' <- transformExpr e
            stmts' <- transformStatements stmts
            appendRest (While e' stmts')
        If cnd thn mElse -> do
            cnd' <- transformExpr cnd
            thn' <- transformStatements thn
            mElse' <- case mElse of
                Nothing -> return Nothing
                Just els -> do
                    els' <- transformStatements els
                    return $ Just els'
            appendRest (If cnd' thn' mElse')
        Let x Nothing -> do
            x' <- newVar
            rest' <- withVar x x' mRest
            return $ Let x' Nothing:rest'
        Let x (Just rhs) -> transformStatements (Let x Nothing:Assign x rhs:rest)
        Assign x rhs -> do
            x' <- envLookup x
            rhs' <- transformExpr rhs
            appendRest (Assign x' rhs')
        Eval e -> do
            e' <- transformExpr e
            appendRest (Eval e')
        Function f args body -> do
            f' <- newVar
            args' <- replicateM (length args) newVar
            let funenv = [(f, f')]
                argsenv = zip args args'
            body' <- withVars (funenv ++ argsenv) (transformStatements body)
            rest' <- withVar f f' mRest
            return (Function f' args' body':rest')
        Return e -> do
            e' <- transformExpr e
            appendRest (Return e')
        Break -> appendRest Break
        Continue -> appendRest Continue

tfProgram :: Program -> Transformer Program 
tfProgram (Program stmts) = Program <$> transformStatements stmts

transformProgram :: Program -> Either TransformerError Program
transformProgram p = evalTransformer (tfProgram p)

evalTransformer :: Transformer a -> Either TransformerError a
evalTransformer m = ma 
    where
        mrws = runTransformer m
        me = fst <$> evalRWST mrws mempty 0
        mi = runExceptT me
        ma = runIdentity mi
