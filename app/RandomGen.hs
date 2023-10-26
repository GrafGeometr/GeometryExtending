module RandomGen where

import System.Random as R
import Control.Monad.State
import Data.Bifunctor (first)
import Control.Applicative
import Control.Monad (MonadPlus)

newtype Rand a = Rand { runRand :: State StdGen [a] } deriving (Functor)

instance Applicative Rand where
    pure = Rand . pure . pure
    Rand f <*> Rand x = Rand $ (<*>) <$> f <*> x

instance Alternative Rand where
    empty = Rand $ pure []
    Rand x <|> Rand y = Rand $ (<>) <$> x <*> y

instance Monad Rand where
    Rand x >>= f = Rand $ x >>= fmap concat . traverse (runRand . f)

instance MonadPlus Rand

random :: Random a => Rand a
random = Rand $ state (first pure . R.random)
