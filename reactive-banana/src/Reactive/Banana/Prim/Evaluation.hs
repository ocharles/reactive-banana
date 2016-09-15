{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, BangPatterns, GADTs #-}
module Reactive.Banana.Prim.Evaluation (
    step
    ) where

import qualified Control.Exception                  as Strict (evaluate)
import           Control.Monad                                (foldM)
import           Control.Monad                                (join)
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import           Data.Functor
import           Data.IORef
import           Data.Maybe
import qualified Data.PQueue.Prio.Min               as Q
import           System.Mem.Weak

import qualified Reactive.Banana.Prim.OrderedBag as OB
import           Reactive.Banana.Prim.Plumbing
import           Reactive.Banana.Prim.Types
import           Reactive.Banana.Prim.Util

type Queue = Q.MinPQueue Level

{-----------------------------------------------------------------------------
    Evaluation step
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> Step
step inputs
        Network{ nTime = time1
        , nOutputs = outputs1
        , nAlwaysP = Just alwaysP   -- we assume that this has been built already
        }
    = {-# SCC step #-} do

    -- evaluate pulses
    ((_, (latchUpdates, outputs)), topologyUpdates, os)
            <- runBuildIO (time1, alwaysP)
            $  runEvalP
            $  evaluatePulses inputs

    doit latchUpdates                           -- update latch values from pulses
    doit topologyUpdates                        -- rearrange graph topology
    let actions = OB.inOrder outputs outputs1   -- EvalO actions in proper order
        state2  = Network
            { nTime    = next time1
            , nOutputs = OB.inserts outputs1 os
            , nAlwaysP = Just alwaysP
            }
    return (runEvalOs $ map snd actions, state2)

runEvalOs :: [EvalO] -> IO ()
runEvalOs = sequence_ . map join

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}
-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> EvalP ()
evaluatePulses roots = do
  fin <- liftIO $ newIORef (return ())
  wrapEvalP $ \r@(time, _) w -> go fin r w =<< insertNodes time roots Q.empty
  liftIO $ join (readIORef fin)
    where
    -- go :: Queue SomeNode -> EvalP ()
    go fin r@(time,_) w q = {-# SCC go #-}
        case ({-# SCC minView #-} Q.minView q) of
            Nothing         -> return ()
            Just (node, q)  -> do
                children <- unwrapEvalP r w (evaluateNode fin node)
                q        <- insertNodes time children q
                go fin r w q

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: IORef (IO ()) -> SomeNode -> EvalP [SomeNode]
evaluateNode cleanup (P p) = {-# SCC evaluateNodeP #-} do
    Pulse{..} <- readRef p
    ma        <- runPulseFunction _evalP
    writePulseP _valueP ma
    liftIO $ modifyIORef cleanup (writeIORef _valueP Nothing >>)
    case ma of
        Nothing -> return []
        Just _  -> liftIO $ deRefWeaks _childrenP
evaluateNode _ (L lw) = {-# SCC evaluateNodeL #-} do
    time           <- askTime
    LatchWrite{..} <- readRef lw
    mlatch         <- liftIO $ deRefWeak _latchLW -- retrieve destination latch
    case mlatch of
        Nothing    -> return ()
        Just latch -> do
            a <- _evalLW                    -- calculate new latch value
            -- liftIO $ Strict.evaluate a      -- see Note [LatchStrictness]
            rememberLatchUpdate $           -- schedule value to be set later
                modify' latch $ \l ->
                    a `seq` l { _seenL = time, _valueL = a }
    return []
evaluateNode _ (O o) = {-# SCC evaluateNodeO #-} do
    debug "evaluateNode O"
    Output{..} <- readRef o
    m          <- _evalO                    -- calculate output action
    rememberOutput $ (o,m)
    return []

runPulseFunction :: PulseFunction a -> EvalP (Maybe a)
runPulseFunction (PulseConst a) = return (Just a)
runPulseFunction (PulseMap f p) = {-# SCC pulseMap #-} fmap f <$> readPulseP p
runPulseFunction (PulseTagLatch x p1) =
  {-# SCC pulseTagLatch #-}
  fmap . const <$> readLatchFutureP x <*> readPulseP p1
runPulseFunction (PulseFilter p1) = {-# SCC pulseFilter #-} join <$> readPulseP p1
runPulseFunction (PulseMapIO f p1) =
  {-# SCC pulseMapIO #-} eval =<< readPulseP p1
  where
    eval (Just x) = Just <$> liftIO (f x)
    eval Nothing  = return Nothing
runPulseFunction (PulseUnionWith f px py) =
  {-# SCC pulseUnionWith #-} eval <$> readPulseP px <*> readPulseP py
  where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing
runPulseFunction (PulseApply f x) = {-# SCC pulseApply #-} fmap <$> readLatchP f <*> readPulseP x
runPulseFunction (PulseExecute p1 b) =
  {-# SCC executeP #-} eval =<< readPulseP p1
  where
    eval (Just x) = Just <$> liftBuildP (x b)
    eval Nothing  = return Nothing
runPulseFunction (PulseJoinLatch lp) = readPulseP =<< readLatchP lp
runPulseFunction (PulseSwitch pp p2) = do
  mnew <- readPulseP pp
  case mnew of
    Nothing -> return ()
    Just new -> liftBuildP $ p2 `changeParent` new
  return Nothing
runPulseFunction (PulseRead p) = readPulseP p

-- | Insert nodes into the queue
-- insertNode :: [SomeNode] -> Queue SomeNode -> EvalP (Queue SomeNode)
insertNodes time = {-# SCC insertNodes #-} go
    where
    go []              q = return q
    go (node@(P p):xs) q = do
        Pulse{..} <- readRef p
        if time <= _seenP
            then go xs q        -- pulse has already been put into the queue once
            else do             -- pulse needs to be scheduled for evaluation
                put p $! (let p = Pulse{..} in p { _seenP = time })
                go xs (Q.insert _levelP node q)
    go (node:xs)      q = go xs (Q.insert ground node q)
            -- O and L nodes have only one parent, so
            -- we can insert them at an arbitrary level
