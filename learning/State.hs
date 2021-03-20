module State where

import Control.Applicative ( Applicative(liftA2) )
import Control.Arrow ( Arrow(second) )
import Control.Monad ( join )
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Identity

newtype StateT s m a = StateT {runState :: s -> m (s, a)}

instance Functor m => Functor (StateT s m) where
    fmap a2b sa = StateT $ \s ->
        let f (s', a) = (s', a2b a)
        in fmap f (runState sa s)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s ->
        pure (s, a)

--     liftA2 ab2c sa sb = StateT $ \s ->
--             let msa = runState sa s
--                 msb2c = fmap (second ab2c) msa
--                 f (s', b2c) = 
--                     let msb = runState sb s'
--                     in fmap (second b2c) msb 
--                 idk = fmap f msb2c
--             in join idk
    liftA2 ab2c sa sb = StateT $ \s -> do
        (s', a) <- runState sa s
        (s'', b) <- runState sb s'
        return (s'', ab2c a b)

instance Monad m => Monad (StateT s m) where
    return = pure
    sa >>= a2sb = StateT $ \s -> do
        (s', a) <- runState sa s
        (s'', b) <- runState (a2sb a) s'
        return (s'', b)
            
instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
      a <- ma
      return (s, a)

get :: Monad m => StateT s m s
get = StateT $ \s ->
    return (s, s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s ->
    return (f s, ())

data BT = L Int | N BT BT deriving (Show)

type State s a = StateT s Identity a

execState :: Functor f => StateT s f b -> s -> f b
execState sa s0 = fmap snd (runState sa s0)

tagTree :: BT -> BT
tagTree bt = runIdentity $ execState (tag bt) 0

tag :: BT -> State Int BT
tag = \case
    L _ -> do
        n <- get
        modify succ
        return (L n)
    N left right -> do
        left' <- tag left
        right' <- tag right
        return (N left' right')