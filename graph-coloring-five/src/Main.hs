{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Math.GraphColor
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn "Missing filename"
    else do
        let filename = head args
        putStrLn "Parsing file"
        possibleGraph <- parseFile filename
        (cs, nodes) <- either parseError perform possibleGraph
        putStrLn "------------------------------------------------"
        putStrLn $ "Total conflicts: " ++ show cs
        print nodes

perform :: (Int, Int, Int, [Edge]) -> IO (Int, [Node])
perform (nc, nn, _, e) = do
    putStrLn $ "Looking for a solution with " ++ show nc ++ " colors"
    run nc nn e

parseError :: String -> IO (Int, [Node])
parseError e = putStrLn e >> return (maxBound, [])
