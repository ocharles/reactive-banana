{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Types (
    -- | Primitive types.
    Event(..), Behavior(..),
    Moment(..), MomentIO(..), MonadMoment(..),
    Future(..),
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix

import qualified Reactive.Banana.Internal.Combinators as Prim

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurrence,

> type Event a = [(Time,a)]

Each pair is called an /event occurrence/.
Note that within a single event stream,
no two event occurrences may happen at the same time.

<<doc/frp-event.png>>
-}
newtype Event a = E { unE :: Prim.Event a }

-- Invariant: The empty list `[]` never occurs as event value.

-- | The function 'fmap' applies a function @f@ to every value.
-- Semantically,
--
-- > fmap :: (a -> b) -> Event a -> Event b
-- > fmap f e = [(time, f a) | (time, a) <- e]
instance Functor Event where
    fmap f = E . Prim.mapE f . unE
    {-# INLINE fmap #-}

{-| @Behavior a@ represents a value that varies in time.
Semantically, you can think of it as a function

> type Behavior a = Time -> a

<<doc/frp-behavior.png>>
-}
newtype Behavior a = B { unB :: Prim.Behavior a }

-- | The function 'pure' returns a value that is constant in time. Semantically,
--
-- > pure     :: a -> Behavior a
-- > pure x    = \time -> x
--
-- The combinator '<*>' applies a time-varying function to a time-varying value.
--
-- > (<*>)    :: Behavior (a -> b) -> Behavior a -> Behavior b
-- > fx <*> bx = \time -> fx time $ bx time
instance Applicative Behavior where
    pure x    = B $ Prim.pureB x
    {-# INLINE pure #-}

    bf <*> bx = B $ Prim.applyB (unB bf) (unB bx)
    {-# INLINE (<*>) #-}

-- | The function 'fmap' applies a function @f@ at every point in time.
-- Semantically,
--
-- > fmap :: (a -> b) -> Behavior a -> Behavior b
-- > fmap f b = \time -> f (b time)
instance Functor Behavior where
    fmap = liftA
    {-# INLINE fmap #-}


-- | The 'Future' monad is just a helper type for the 'changes' function.
--
-- A value of type @Future a@ is only available in the context
-- of a 'reactimate' but not during event processing.
newtype Future a = F { unF :: Prim.Future a }
  deriving (Functor, Applicative, Monad)


{-| The 'Moment' monad denotes a /pure/ computation that happens
at one particular moment in time. Semantically, it is a reader monad

> type Moment a = Time -> a

When run, the argument tells the time at which this computation happens.

Note that in this context, /time/ really means to /logical time/.
Of course, every calculation on a computer takes some
amount of wall-clock time to complete.
Instead, what is meant here is the time as it relates to
'Event's and 'Behavior's.
We use the fiction that every calculation within the 'Moment'
monad takes zero /logical time/ to perform.
-}
newtype Moment a = M { unM :: Prim.Moment a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

{-| The 'MomentIO' monad is used to add inputs and outputs
to an event network.
-}
newtype MomentIO a = MIO { unMIO :: Prim.Moment a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

{-| An instance of the 'MonadMoment' class denotes a computation
that happens at one particular moment in time.
Unlike the 'Moment' monad, it need not be pure anymore.
-}
class Monad m => MonadMoment m where
    liftMoment :: Moment a -> m a

instance MonadMoment Moment   where
  liftMoment = id
  {-# INLINE liftMoment #-}

instance MonadMoment MomentIO where
  liftMoment = MIO . unM
  {-# INLINE liftMoment #-}
