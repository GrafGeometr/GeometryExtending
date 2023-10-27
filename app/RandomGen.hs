module RandomGen where

import System.Random



import Control.Monad.State
import Data.Real.Constructible (Construct)
import Data.Bifunctor (first)


newtype Rand a = Rand (State StdGen a) deriving (Functor, Applicative, Monad)

runRand :: Rand a -> IO a
runRand (Rand x) = evalState x <$> newStdGen

random :: Int -> Int -> Rand Construct
random x y = Rand . state $ first fromIntegral . randomR (x, y)




