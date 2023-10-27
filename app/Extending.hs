module Extending where

import Types
import RandomGen

import qualified Data.Map as Map
import Control.Monad (replicateM)


getShps :: Map.Map String Shape -> [String] -> [Shape]
getShps m = fmap (m Map.!)



checkFinal :: Map.Map String Shape -> [Command] -> Conclusion -> Rand Bool
checkFinal m [] (Conclusion x f) = pure . not . f $ getShps m x
checkFinal m (Command r x f : xs) c = last (f $ getShps m x) >>= \h -> checkFinal (Map.insert r h m) xs c

check :: Map.Map String Shape -> [Command] -> Conclusion -> Rand Bool
check _ [] (Conclusion _ _) = pure True
check m (Command r x f : xs) c = do
    ps <- sequence (f $ getShps m x)
    ys <- traverse (\p -> checkFinal (Map.insert r p m) xs c) $ init ps
    y <- check (Map.insert r (last ps) m) xs c
    return $ and ys && y

check10 :: [Command] -> Conclusion -> Rand Bool
check10 xs c = not . or <$> replicateM 10 (check Map.empty xs c)
