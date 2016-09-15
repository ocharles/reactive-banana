{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Reactive.Banana.Prim.Plumbing
    ( neverP, newPulse, newLatch, cachedLatch
    , dependOn, keepAlive, changeParent
    , getValueL
    , readPulseP, readLatchP, readLatchFutureP, liftBuildP,
    )
import qualified Reactive.Banana.Prim.Plumbing (pureL)
import           Reactive.Banana.Prim.Types    (Latch, Future, Pulse, Build, PulseFunction(..))

import Debug.Trace
-- debug s = trace s
debug s = id

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p1 = do
    p2 <- newPulse "mapP" (PulseMap f p1)
    p2 `dependOn` p1
    return p2

-- | Tag a 'Pulse' with future values of a 'Latch'.
--
-- This is in contrast to 'applyP' which applies the current value
-- of a 'Latch' to a pulse.
tagFuture :: Latch a -> Pulse b -> Build (Pulse (Future a))
tagFuture x p1 = do
    p2 <- newPulse "tagFuture" (PulseTagLatch x p1)
    p2 `dependOn` p1
    return p2

filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p1 = do
    p2 <- newPulse "filterJustP" (PulseFilter p1)
    p2 `dependOn` p1
    return p2

unsafeMapIOP :: (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p1 = do
        p2 <- newPulse "unsafeMapIOP" (PulseMapIO f p1)

        p2 `dependOn` p1
        return p2

unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f px py = do
        p <- newPulse "unionWithP" (PulseUnionWith f px py)
        p `dependOn` px
        p `dependOn` py
        return p

-- See note [LatchRecursion]
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = do
    p <- newPulse "applyP" (PulseApply f x)
    p `dependOn` x
    return p

pureL :: a -> Latch a
pureL = Reactive.Banana.Prim.Plumbing.pureL

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Latch a -> Latch b
mapL f lx = cachedLatch $ {-# SCC mapL #-} f <$> getValueL lx

applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL lf lx = cachedLatch $
    {-# SCC applyL #-} getValueL lf <*> getValueL lx

accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    (updateOn, x) <- newLatch a
    p2 <- applyP (mapL (\x f -> f x) x) p1
    updateOn p2
    return (x,p2)

-- specialization of accumL
stepperL :: a -> Pulse a -> Build (Latch a)
stepperL a p = do
    (updateOn, x) <- newLatch a
    updateOn p
    return x

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
switchL :: Latch a -> Pulse (Latch a) -> Build (Latch a)
switchL l pl = mdo
    x <- stepperL l pl
    return $ cachedLatch $ getValueL x >>= getValueL

executeP :: Pulse (b -> Build a) -> b -> Build (Pulse a)
executeP p1 b = do
        p2 <- newPulse "executeP" (PulseExecute p1 b)
        p2 `dependOn` p1
        return p2

switchP :: Pulse (Pulse a) -> Build (Pulse a)
switchP pp = mdo
    never <- neverP
    lp    <- stepperL never pp
    p1 <- newPulse "switchP_in" (PulseSwitch pp p2)
    p1 `dependOn` pp
    p2 <- newPulse "switchP_out" (PulseJoinLatch lp)
    p2 `keepAlive` p1
    return p2
