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
        either parseError perform possibleGraph
        return ()

perform :: (Int, Int, Int, [Edge]) -> IO [Node]
perform (nc, nn, _, e) = findBest nc nn e

parseError :: String -> IO [Node]
parseError e = putStrLn e >> return []
