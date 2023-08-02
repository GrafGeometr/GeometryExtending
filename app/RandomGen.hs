module RandomGen where

import System.Random



import Control.Monad.State


newtype Rand a = Rand (State StdGen a) deriving (Functor, Applicative, Monad)

runRand :: Rand a -> IO a
runRand (Rand x) = evalState x <$> newStdGen

random :: Double -> Double -> Rand Double
random x y = Rand . state $ randomR (x, y)




