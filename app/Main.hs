module Main where

import Parser (parseTheorem)
import System.Environment (getArgs)
import RandomGen (runRand)
import Extending
import System.Directory (renameFile, listDirectory)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad (forM_, unless)

main :: IO ()
main = do
    [dir] <- getArgs
    filesWithBad <- listDirectory dir
    forM_ (filter (\s -> "BAD_" `isPrefixOf` s) filesWithBad) $ \file ->
        renameFile (dir <> file) (dir <> drop 4 file)
    files <- listDirectory dir
    forM_ (filter (\s -> ".geo" `isSuffixOf` s) files) $ \file -> do
        let arg = dir <> file
        s <- readFile arg
        putStrLn $ "   !Started: " <> file
        case parseTheorem file $ unlines . fmap tail . filter ("!" `isPrefixOf`) . lines $ s of
            Left err -> print err
            Right (cs, cc) -> do
                res <- runRand $ check10 cs cc
                unless res $ renameFile arg (dir <> "BAD_" <> file)
