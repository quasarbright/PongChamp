{-# LANGUAGE FlexibleContexts #-}
module WellFormedness where

import Control.Monad.RWS.Strict
import Data.Set(Set)
import AST

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

checkExpr :: Expr -> Checker ()
checkExpr = \case
    Var x -> do
        vars <- ask
        unless (x `elem` vars) (throw (UnboundVar x))
    Number{} -> nothing
    String{} -> nothing
    Bool{} -> nothing
    






