{-# LANGUAGE FlexibleContexts #-}
module WellFormedness where

import Control.Monad.RWS.Strict
import Data.Set(Set)
import qualified Data.Set as Set
import AST
import Data.Maybe (fromMaybe)

type Env = Set String

data WFError
    = UnboundVar String
    | DupVar String
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

withVar :: MonadReader Env m => String -> m a -> m a
withVar x = local (Set.insert x)

withVars :: MonadReader Env m => [String] -> m a -> m a
withVars xs = local (Set.union (Set.fromList xs))

wfExpr :: Expr -> Checker ()
wfExpr = \case
    Var x -> do
        vars <- ask
        unless (x `elem` vars) (throw (UnboundVar x))
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
    While cond body -> wfExpr cond >> wfStatements body >> mRest
    If cond thn mEls -> wfExpr cond >> wfStatements (thn ++ els) >> mRest
        where els = fromMaybe [] mEls
    Assign x rhs -> wfExpr rhs >> withVar x mRest
    Eval e -> wfExpr e >> mRest
    Function f args body -> mArgs >> withVars (f:args) (wfStatements body) >> withVar f mRest
        where
            mArgs = checkDups args -- reverse to get the errors in the right order
            checkDups :: [String] -> Checker ()
            checkDups [] = nothing
            checkDups (x:xs) = when (x `elem` xs) (throw (DupVar x)) >> checkDups xs

wfProgram :: Program -> Checker ()
wfProgram (Program stmts) = wfStatements stmts

checkProgram :: Program -> [WFError]
checkProgram p = snd $ evalRWS (runChecker (wfProgram p)) mempty ()


