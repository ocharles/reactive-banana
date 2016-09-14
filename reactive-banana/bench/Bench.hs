module Main where

import Control.Monad
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks
import Criterion.Main as C

main :: IO ()
main = do
  C.defaultMain
    [ C.env
        (do (incrementAh, increment) <- newAddHandler
            out <- newIORef 0
            network <-
              compile $ do
                onInc <- fromAddHandler incrementAh
                n <- accumE 0 (succ <$ onInc)
                reactimate (writeIORef out <$> n)
            actuate network
            return increment)
        (\io -> C.bench "counter" $ C.whnfIO $ io ())
    ]
