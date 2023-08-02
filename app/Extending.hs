module Extending where


import Debug.Trace


import Types
import RandomGen

import qualified Data.Map as Map
import Control.Monad (replicateM)


getPts :: Map.Map String Point -> [String] -> [Point]
getPts m args = fmap (m Map.!) args



checkFinal :: Map.Map String Point -> [Command] -> Conclusion -> Rand Bool
checkFinal m [] (Conclusion x f) = pure . not . f $ getPts m x
checkFinal m (Command r x f : xs) c = last (f $ getPts m x) >>= \h -> checkFinal (Map.insert r h m) xs c

check :: Map.Map String Point -> [Command] -> Conclusion -> Rand Bool
check m [] (Conclusion x f) = if f $ getPts m x then pure True else trace "Theorem check failed" $ pure True
check m (Command r x f : xs) c = do
    ps <- sequence (f $ getPts m x)
    ys <- traverse (\p -> checkFinal (Map.insert r p m) xs c) $ init ps
    y <- check (Map.insert r (last ps) m) xs c
    return $ and ys && y

check5 :: [Command] -> Conclusion -> Rand Bool
check5 xs c = ((>=2) . length . (filter id)) <$> replicateM 10 (check Map.empty xs c)

--check5 xs c = or <$> replicateM 20 (check Map.empty xs c)
