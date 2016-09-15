{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, RecursiveDo, BangPatterns #-}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad                                (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader         as Reader
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import           Data.Function                                (on)
import           Data.Functor
import           Data.IORef
import           Data.List                                    (sortBy)
import           Data.Monoid
import           System.IO.Unsafe

import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Types
import           Reactive.Banana.Prim.Util

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> PulseFunction a -> Build (Pulse a)
newPulse name eval = liftIO $ do
    value <- newIORef Nothing
    newRef $ Pulse
        { _valueP      = value
        , _seenP     = agesAgo
        , _evalP     = eval
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = name
        }

{-
* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

-}

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = liftIO $ do
    value <- newIORef Nothing
    newRef $ Pulse
        { _valueP      = value
        , _seenP     = agesAgo
        , _evalP     = PulseNever
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "neverP"
        }

-- | Return a 'Latch' that has a constant value
pureL :: a -> Latch a
pureL a = unsafePerformIO $ newRef $ Latch
    { _seenL  = beginning
    , _valueL = a
    , _evalL  = return a
    }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: a -> Build (Pulse a -> Build (), Latch a)
newLatch a = mdo
    latch <- liftIO $ newRef $ Latch
        { _seenL  = beginning
        , _valueL = a
        , _evalL  = do
            Latch {..} <- readRef latch
            RW.tell _seenL  -- indicate timestamp
            return _valueL  -- indicate value
        }
    let
        err        = error "incorrect Latch write"
        updateOn p = do
            w  <- liftIO $ mkWeakRefValue latch latch 
            lw <- liftIO $ newRef $ LatchWrite
                { _evalLW  = maybe err id <$> readPulseP p
                , _latchLW = w
                }
            -- writer is alive only as long as the latch is alive
            _  <- liftIO $ mkWeakRefValue latch lw
            (P p) `addChild` (L lw)
    
    return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval = unsafePerformIO $ mdo
    latch <- newRef $ Latch
        { _seenL  = agesAgo
        , _valueL = error "Undefined value of a cached latch."
        , _evalL  = do
            Latch{..} <- liftIO $ readRef latch
            -- calculate current value (lazy!) with timestamp
            (a,time)  <- RW.listen eval
            liftIO $ if time <= _seenL
                then return _valueL     -- return old value
                else do                 -- update value
                    let _seenL  = time
                    let _valueL = a
                    a `seq` put latch (Latch {..})
                    return a
        }
    return latch

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
    o <- liftIO $ newRef $ Output
        { _evalO = p
        }
    (P p) `addChild` (O o)
    RW.tell $ BuildW (mempty, [o], mempty, mempty)
{-# INLINE addOutput #-}

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: BuildR -> BuildIO a -> IO (a, Action, [Output])
runBuildIO i m = {-# SCC runBuild #-} do
        (a, BuildW (topologyUpdates, os, liftIOLaters, _)) <- unfold mempty m
        doit $ liftIOLaters          -- execute late IOs
        return (a,Action $ Deps.buildDependencies topologyUpdates,os)
    where
    -- Recursively execute the  buildLater  calls.
    unfold :: BuildW -> BuildIO a -> IO (a, BuildW)
    unfold w m = do
        (a, BuildW (w1, w2, w3, later)) <- RW.runReaderWriterIOT m i
        let w' = w <> BuildW (w1,w2,w3,mempty)
        w'' <- case later of
            Just m  -> snd <$> unfold w' m
            Nothing -> return w'
        return (a,w'')
{-# INLINE runBuildIO #-}

buildLater :: Build () -> Build ()
buildLater x = RW.tell $ BuildW (mempty, mempty, mempty, Just x)
{-# INLINE buildLater #-}

-- | Pretend to return a value right now,
-- but do not actually calculate it until later.
--
-- NOTE: Accessing the value before it's written leads to an error.
--
-- FIXME: Is there a way to have the value calculate on demand?
buildLaterReadNow :: Build a -> Build a
buildLaterReadNow m = do
    ref <- liftIO $ newIORef $
        error "buildLaterReadNow: Trying to read before it is written."
    buildLater $ m >>= liftIO . writeIORef ref
    liftIO $ unsafeInterleaveIO $ readIORef ref
{-# INLINE buildLaterReadNow #-}

liftBuild :: Build a -> BuildIO a
liftBuild = id
{-# INLINE liftBuild #-}

getTimeB :: Build Time
getTimeB = (\(x,_) -> x) <$> RW.ask
{-# INLINE getTimeB #-}

alwaysP :: Build (Pulse ())
alwaysP = (\(_,x) -> x) <$> RW.ask
{-# INLINE alwaysP #-}

readLatchB :: Latch a -> Build a
readLatchB = liftIO . readLatchIO
{-# INLINE readLatchB #-}

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = (P parent) `addChild` (P child)
{-# INLINE dependOn #-}

keepAlive :: Pulse child -> Pulse parent -> Build ()
keepAlive child parent = liftIO $ mkWeakRefValue child parent >> return ()
{-# INLINE keepAlive #-}

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
    RW.tell $ BuildW (Deps.addChild parent child, mempty, mempty, mempty)
{-# INLINE addChild #-}

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent node parent =
    RW.tell $ BuildW (Deps.changeParent node parent, mempty, mempty, mempty)
{-# INLINE changeParent #-}

liftIOLater :: IO () -> Build ()
liftIOLater x = RW.tell $ BuildW (mempty, mempty, Action x, mempty)
{-# INLINE liftIOLater #-}

{-----------------------------------------------------------------------------
    EvalL monad
------------------------------------------------------------------------------}
-- | Evaluate a latch (-computation) at the latest time,
-- but discard timestamp information.
readLatchIO :: Latch a -> IO a
readLatchIO latch = do
    Latch{..} <- readRef latch
    liftIO $ fst <$> RW.runReaderWriterIOT _evalL ()
{-# INLINE readLatchIO #-}

getValueL :: Latch a -> EvalL a
getValueL latch = do
    Latch{..} <- readRef latch
    _evalL
{-# INLINE getValueL #-}

{-----------------------------------------------------------------------------
    EvalP monad
------------------------------------------------------------------------------}
runEvalP :: EvalP a -> Build (a, EvalPW)
runEvalP m = RW.readerWriterIOT $ \r2 -> do
    (a,(w1,w2)) <- RW.runReaderWriterIOT m r2
    return ((a,w1), w2)
{-# INLINE runEvalP #-}

liftBuildP :: Build a -> EvalP a
liftBuildP m = RW.readerWriterIOT $ \r2 -> do
    (a,w2) <- RW.runReaderWriterIOT m r2
    return (a,(mempty,w2))

askTime :: EvalP Time
askTime = fst <$> RW.ask
{-# INLINE askTime #-}

--readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP p = do
    Pulse{..} <- readRef p
    liftIO (readIORef _valueP)
{-# INLINE readPulseP #-}

--writePulseP :: IORef (Maybe a) -> Maybe a -> EvalP ()
writePulseP key a = liftIO (writeIORef key a)
{-# INLINE writePulseP #-}

readLatchP :: Latch a -> EvalP a
readLatchP = liftBuildP . readLatchB
{-# INLINE readLatchP #-}

readLatchFutureP :: Monad m => Latch a -> m (Future a)
readLatchFutureP = return . readLatchIO
{-# INLINE readLatchFutureP #-}

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RW.tell ((Action x,mempty),mempty)
{-# INLINE rememberLatchUpdate #-}

rememberOutput :: (Output, EvalO) -> EvalP ()
rememberOutput x = RW.tell ((mempty,[x]),mempty)
{-# INLINE rememberOutput #-}

-- worker wrapper to break sharing and support better inlining
unwrapEvalP r w m = RW.run m r w
{-# INLINE unwrapEvalP #-}
wrapEvalP   m   = RW.ReaderWriterIOT m
{-# INLINE wrapEvalP #-}
