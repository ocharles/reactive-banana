module Control.Monad.Trans.RWSIO (
    -- * Synopsis
    -- | An implementation of the reader/writer/state monad transformer
    -- using an 'IORef'.

    -- * Documentation
    RWSIOT(..), Tuple(..), rwsT, runRWSIOT, tell, ask, get, put,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Monoid

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
data Tuple r w s = Tuple !r !(IORef w) !(IORef s)

newtype RWSIOT r w s a = R { run :: Tuple r w s -> IO a }

instance Functor (RWSIOT r w s) where
  fmap = fmapR
  {-# INLINE fmap #-}

instance Applicative (RWSIOT r w s) where
    pure  = pureR
    {-# INLINE pure #-}
    (<*>) = apR
    {-# INLINE (<*>) #-}

instance Monad (RWSIOT r w s) where
    return = returnR
    {-# INLINE return #-}
    (>>=)  = bindR
    {-# INLINE (>>=) #-}

instance MonadFix (RWSIOT r w s) where
  mfix = mfixR
  {-# INLINE mfix #-}

instance MonadIO (RWSIOT r w s)   where
  liftIO = liftIOR
  {-# INLINE liftIO #-}

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR m = R $ \_ -> liftIO m
{-# INLINE liftIOR #-}

liftR   m = R $ \_ -> m
{-# INLINE liftR #-}

fmapR f m = R $ \x -> fmap f (run m x)
{-# INLINE fmapR #-}

returnR a = R $ \_ -> return a
{-# INLINE returnR #-}

bindR m k = R $ \x -> run m x >>= \a -> run (k a) x
{-# INLINE bindR #-}

mfixR f   = R $ \x -> mfix (\a -> run (f a) x)
{-# INLINE mfixR #-}

pureR a   = R $ \_ -> pure a
{-# INLINE pureR #-}

apR f a   = R $ \x -> run f x <*> run a x
{-# INLINE apR #-}

rwsT :: (Monoid w) => (r -> s -> IO (a, s, w)) -> RWSIOT r w s a
rwsT f = do
    r <- ask
    s <- get
    (a,s,w) <- liftIO (f r s)
    put  s
    tell w
    return a
{-# INLINE rwsT #-}

runRWSIOT :: (Monoid w) => RWSIOT r w s a -> (r -> s -> IO (a,s,w))
runRWSIOT m r s = do
    w' <- newIORef mempty
    s' <- newIORef s
    a  <- run m (Tuple r w' s')
    s  <- readIORef s'
    w  <- readIORef w'
    return (a,s,w)
{-# INLINE runRWSIOT #-}

tell :: (Monoid w) => w -> RWSIOT r w s ()
tell w = R $ \(Tuple _ w' _) -> modifyIORef w' (`mappend` w)
{-# INLINE tell #-}

ask :: RWSIOT r w s r
ask = R $ \(Tuple r _ _) -> return r
{-# INLINE ask #-}

get :: RWSIOT r w s s
get = R $ \(Tuple _ _ s') -> readIORef s'
{-# INLINE get #-}

put :: s -> RWSIOT r w s ()
put s = R $ \(Tuple _ _ s') -> writeIORef s' s
{-# INLINE put #-}

test :: RWSIOT String String () ()
test = do
    c <- ask
    tell c
