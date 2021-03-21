{-# LANGUAGE FlexibleContexts #-}
module WellFormedness where

import Control.Monad.RWS.Strict
import Data.Set(Set)
import qualified Data.Set as Set
import AST
import Data.Maybe (fromMaybe)
import Control.Arrow

data Context = Context{isInFunction :: Bool, isInLoop :: Bool} deriving(Eq, Ord, Show)

type Env = (Set String, Context)

data WFError
    = UnboundVar String
    | DupVar String
    | BadReturn
    | BadBreak
    | BadContinue
    deriving(Eq, Ord, Show)

newtype Checker a = Checker {runChecker :: (RWS Env [WFError] ()) a}
    deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadWriter [WFError]
    )

throw :: MonadWriter [a] m => a -> m ()
throw = tell . (:[])

nothing :: Monad m => m ()
nothing = return ()

askVars :: Checker (Set String)
askVars = asks fst

askContext :: Checker Context
askContext = asks snd

localVars :: (Set String -> Set String) -> Checker a -> Checker a
localVars f = local (first f)

localContext :: (Context -> Context) -> Checker a -> Checker a
localContext f = local (second f)

withVar :: String -> Checker a -> Checker a
withVar x = localVars (Set.insert x)

withVars :: [String] -> Checker a -> Checker a
withVars xs = localVars (Set.union (Set.fromList xs))

inFunction :: Checker a -> Checker a
inFunction = localContext $ \ctx -> ctx{isInFunction=True}

inLoop :: Checker a -> Checker a
inLoop = localContext $ \ctx -> ctx{isInLoop=True}

assertInScope :: String -> Checker ()
assertInScope x = do
    vars <- askVars
    unless (x `elem` vars) (throw (UnboundVar x))

wfExpr :: Expr -> Checker ()
wfExpr = \case
    Var x -> assertInScope x
    Number{} -> nothing
    String{} -> nothing
    Bool{} -> nothing
    Unop _ e -> wfExpr e
    Binop _ l r -> mapM_ wfExpr [l, r]
    Call f args -> mapM_ wfExpr (f:args)

wfStatements :: [Statement] -> Checker ()
wfStatements [] = nothing
wfStatements (s_:rest) =
    let mRest = wfStatements rest
    in case s_ of
    While cond body -> wfExpr cond >> inLoop (wfStatements body) >> mRest
    If cond thn mEls -> wfExpr cond >> wfStatements (thn ++ els) >> mRest
        where els = fromMaybe [] mEls
    Let x Nothing -> withVar x mRest
    Let x (Just rhs) -> wfStatements (Let x Nothing:Assign x rhs:rest)
    Assign x rhs -> assertInScope x >> wfExpr rhs >> mRest
    Eval e -> wfExpr e >> mRest
    Function f args body -> mArgs >> inFunction (withVars (f:args) (wfStatements body)) >> withVar f mRest
        where
            mArgs = checkDups args -- reverse to get the errors in the right order
            checkDups :: [String] -> Checker ()
            checkDups [] = nothing
            checkDups (x:xs) = when (x `elem` xs) (throw (DupVar x)) >> checkDups xs
    Return e -> wfExpr e >> mRest
    Break -> undefined 
    _ -> undefined 

wfProgram :: Program -> Checker ()
wfProgram (Program stmts) = wfStatements stmts

checkProgram :: Program -> [WFError]
checkProgram p = snd $ evalRWS (runChecker (wfProgram p)) (mempty, Context False False) ()


