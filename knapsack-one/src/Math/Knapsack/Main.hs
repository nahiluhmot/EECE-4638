module Main (main) where

import Math.Knapsack
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn "Missing filename"
    else do
        let filename = head args
        print "Parsing file"
        possibleKnapsack <- parseFile filename
        objs <- either parseError perform possibleKnapsack
        print objs

perform :: (Int, Int, [Object]) -> IO [Object]
perform (_, c, os) = do
    print ("Looking for set of objects less than " ++ show c)
    findBest os c

parseError :: String -> IO [Object]
parseError e = putStrLn e >> return []
