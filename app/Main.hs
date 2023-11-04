module Main where

import Parser (parseTheorem)
import System.Environment (getArgs)
import RandomGen (runRand)
import Extending
import System.Directory (renameFile, listDirectory)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (forM_, unless)
import Control.Exception (try, SomeException)
import Control.Concurrent.MSem
import Control.Concurrent.Async (forConcurrently_)
import System.Timeout (timeout)



mapPool :: (Integral i, Foldable f) => i -> (a -> IO b) -> f a -> IO ()
mapPool mx f xs = do
    sem <- new mx
    forConcurrently_ xs (with sem . f)



main :: IO ()
main = do
    [dir] <- getArgs
    filesWithBad <- listDirectory dir
    forM_ (filter (\s -> "BAD_" `isPrefixOf` s) filesWithBad) $ \file ->
        renameFile (dir <> file) (dir <> drop 4 file)
    files <- listDirectory dir
    let filenames = (filter (\s -> ".geo" `isSuffixOf` s) files)
    
    mapPool 8 id $ (\file -> do
            putStrLn $ "   !Started: " <> file

            result <- try (do
                let arg = dir <> file
                s <- readFile arg
                
                res <- timeout (1 * 1000000) $ case parseTheorem file $ unlines . fmap tail . filter ("!" `isPrefixOf`) . lines $ s of
                    Left err -> print err
                    Right (cs, cc) -> do
                        res <- runRand $ check10 cs cc
                        unless res $ renameFile arg (dir <> "BAD_" <> file)
                
                case res of
                    Just _ -> return ()
                    Nothing -> putStrLn $ "   !Timeout: " <> file) :: IO (Either SomeException ())
            
            case result of 
                Left err -> print err
                Right _ -> putStrLn $ "   !Finished: " <> file) <$> filenames
    
    
    
