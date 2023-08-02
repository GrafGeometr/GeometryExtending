{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Complex
import RandomGen


type Point = Complex Double

data Command = Command { res :: String, args :: [String], action :: [Point] -> [Rand Point] }

data Conclusion = Conclusion { arguments :: [String], factChecker :: [Point] -> Bool }