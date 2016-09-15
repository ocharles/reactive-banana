{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Trans.ReaderWriterIO (
    -- * Synopsis
    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.
    
    -- * Documentation
    ReaderWriterIOT(..), readerWriterIOT, runReaderWriterIOT, tell, listen, ask, local,
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
newtype ReaderWriterIOT r w a = ReaderWriterIOT { run :: r -> IORef w -> IO a }

instance Functor (ReaderWriterIOT r w) where
  fmap = fmapR
  {-# INLINE fmap #-}

instance Applicative (ReaderWriterIOT r w) where
  pure = pureR
  {-# INLINE pure #-}
  (<*>) = apR
  {-# INLINE (<*>) #-}

instance Monad (ReaderWriterIOT r w) where
  return = returnR
  {-# INLINE return #-}
  (>>=) = bindR
  {-# INLINE (>>=) #-}

instance MonadFix (ReaderWriterIOT r w) where
  mfix = mfixR
  {-# INLINE mfix #-}

instance MonadIO (ReaderWriterIOT r w) where
  liftIO = liftIOR
  {-# INLINE liftIO #-}

instance (a ~ ()) =>
         Monoid (ReaderWriterIOT r w a) where
  mempty = return ()
  {-# INLINE mempty #-}
  mx `mappend` my = mx >> my
  {-# INLINE mappend #-}

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR m = ReaderWriterIOT $ \x y -> liftIO m
{-# INLINE liftIOR #-}

liftR m = ReaderWriterIOT $ \x y -> m
{-# INLINE liftR #-}

fmapR f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)
{-# INLINE fmapR #-}

returnR a = ReaderWriterIOT $ \_ _ -> return a
{-# INLINE returnR #-}

bindR m k = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y
{-# INLINE bindR #-}

mfixR f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)
{-# INLINE mfixR #-}

pureR a = ReaderWriterIOT $ \_ _ -> pure a
{-# INLINE pureR #-}

apR f a = ReaderWriterIOT $ \x y -> run f x y <*> run a x y
{-# INLINE apR #-}

readerWriterIOT :: (Monoid w) =>
    (r -> IO (a, w)) -> ReaderWriterIOT r w a
readerWriterIOT f = do
    r <- ask
    (a,w) <- liftIOR $ f r
    tell w
    return a
{-# INLINE readerWriterIOT #-}

runReaderWriterIOT
  :: (Monoid w)
  => ReaderWriterIOT r w a -> r -> IO (a, w)
runReaderWriterIOT m r = do
  ref <- newIORef mempty
  a <- run m r ref
  w <- readIORef ref
  return (a, w)
{-# INLINE runReaderWriterIOT #-}

tell
  :: (Monoid w)
  => w -> ReaderWriterIOT r w ()
tell w = ReaderWriterIOT $ \_ ref -> modifyIORef ref (`mappend` w)
{-# INLINE tell #-}

listen
  :: (Monoid w)
  => ReaderWriterIOT r w a -> ReaderWriterIOT r w (a, w)
listen m =
  ReaderWriterIOT $ \r ref -> do
    a <- run m r ref
    w <- readIORef ref
    return (a, w)
{-# INLINE listen #-}

local :: (r -> r) -> ReaderWriterIOT r w a -> ReaderWriterIOT r w a
local f m = ReaderWriterIOT $ \r ref -> run m (f r) ref
{-# INLINE local #-}

ask :: ReaderWriterIOT r w r
ask = ReaderWriterIOT $ \r _ -> return r
{-# INLINE ask #-}

test :: ReaderWriterIOT String String ()
test = do
    c <- ask
    tell c
