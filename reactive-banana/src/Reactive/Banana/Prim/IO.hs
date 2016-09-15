{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.IO where

import           Control.Monad.IO.Class
import           Data.Functor
import           Data.IORef

import Reactive.Banana.Prim.Combinators (mapP)
import Reactive.Banana.Prim.Evaluation  (step)
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util

debug s = id

{-----------------------------------------------------------------------------
    Primitives connecting to the outside world
------------------------------------------------------------------------------}
-- | Create a new pulse in the network and a function to trigger it.
--
-- Together with 'addHandler', this function can be used to operate with
-- pulses as with standard callback-based events.
newInput :: Build (Pulse a, a -> Step)
newInput = mdo
    always <- alwaysP
    value <- liftIO (newIORef Nothing)
    pulse  <- liftIO $ newRef $ Pulse
        { _valueP    = value
        , _seenP     = agesAgo
        , _evalP     = PulseRead pulse    -- get its own value
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "newInput"
        }
    -- Also add the  alwaysP  pulse to the inputs.
    let run a n = do
          liftIO (writeIORef value (Just a))
          s <- step [P pulse, P always] n
          liftIO (writeIORef value Nothing)
          return s

    return (pulse, run)

-- | Register a handler to be executed whenever a pulse occurs.
--
-- The pulse may refer to future latch values.
addHandler :: Pulse (Future a) -> (a -> IO ()) -> Build ()
addHandler p1 f = do
    p2 <- mapP (fmap f) p1
    addOutput p2

-- | Read the value of a 'Latch' at a particular moment in time.
readLatch :: Latch a -> Build a
readLatch = readLatchB
