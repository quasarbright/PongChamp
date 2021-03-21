module Interpreter where

import Control.Monad.Except
import Control.Monad.RWS.Strict
type Env = ()
type Store = ()
type RuntimeError = ()

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

evalInterpreter :: Interpreter a -> IO (Either RuntimeError a)
evalInterpreter m = mio 
    where
        mrws = runInterpreter m
        me = fst <$> evalRWST mrws () ()
        mio = runExceptT me

