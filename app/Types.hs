{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types where

import RandomGen
import Data.Modular
import System.Random (Random(..))
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Control.Applicative

newtype Fin = Fin (Mod Int 10_007) deriving (Eq, Ord, Show, Enum, Bounded, Num, Fractional)

instance Random Fin where
    randomR (Fin a, Fin b) = first fromIntegral . randomR (unMod a, unMod b)
    random = randomR (minBound, maxBound)

rootsTable :: Map.Map Fin Fin
rootsTable = Map.fromList $ (\x -> (x * x, x)) <$> [0..]

dsqrt :: Fin -> Rand Fin
dsqrt = maybe empty (\x -> asum $ fmap pure [x, -x]) . (rootsTable Map.!?)

infixl 6 :+
data Complex a = a :+ a deriving (Eq, Show)

instance Num a => Num (Complex a) where
    fromInteger x = fromInteger x :+ 0
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    negate (a :+ b) = negate a :+ negate b

instance Fractional a => Fractional (Complex a) where
    fromRational x = fromRational x :+ 0
    recip (a :+ b) = let n = a * a + b * b in (a / n) :+ (- b / n)

type Point = Complex Fin


-- ~coef * z + coef * ~z + free = 0
data Line = Line { coef :: Complex Fin, free :: Fin} deriving Show

data Circle = Circle { center :: Point, radiusSqr :: Fin} deriving Show

data Shape = PointShape Point | LineShape Line | CircleShape Circle deriving Show



class IsShape a where
    fromShape :: Shape -> a
    toShape :: a -> Shape

instance IsShape Point where
    fromShape (PointShape p) = p
    toShape = PointShape

instance IsShape Line where
    fromShape (LineShape l) = l
    toShape = LineShape

instance IsShape Circle where
    fromShape (CircleShape c) = c
    toShape = CircleShape



class Builder f where
    mkBuilder :: f -> [Shape] -> [Rand Shape]


instance IsShape a => Builder [Rand a] where
    mkBuilder :: IsShape a => [Rand a] -> [Shape] -> [Rand Shape]
    mkBuilder randList _ = fmap (>>= pure . toShape) randList


instance (IsShape a, Builder f) => Builder (a -> f) where
    mkBuilder :: (IsShape a, Builder f) => (a -> f) -> [Shape] -> [Rand Shape]
    mkBuilder f (x : xs) = mkBuilder (f $ fromShape x) xs


class Checker f where
    mkChecker :: f -> [Shape] -> Bool

instance Checker Bool where
    mkChecker :: Bool -> [Shape] -> Bool
    mkChecker = const

instance (IsShape a, Checker f) => Checker (a -> f) where
    mkChecker :: (IsShape a, Checker f) => (a -> f) -> [Shape] -> Bool
    mkChecker f (x : xs) = mkChecker (f $ fromShape x) xs


data Command = Command { res :: String, args :: [String], action :: [Shape] -> [Rand Shape] }

data Conclusion = Conclusion { arguments :: [String], factChecker :: [Shape] -> Bool }

instance Show Command where
    show (Command r as _) = "Command " <> r <> " " <> show as

instance Show Conclusion where
    show (Conclusion as _) = "Conclusion " <> show as