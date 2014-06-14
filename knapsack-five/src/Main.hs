module Main (main) where

import Math.Knapsack as K
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn "Missing filename"
    else do
        let filename = head args
        putStrLn "Parsing file"
        possibleKnapsack <- parseFile filename
        objs <- either parseError perform possibleKnapsack

        putStrLn "------------------------------------------------"
        putStrLn $ "Total value: " ++ (show . sum $ map value objs)
        putStrLn $ "Total cost: " ++ (show . sum $ map cost objs)
        mapM_ print objs

perform :: (Int, Int, [Object]) -> IO [Object]
perform (_, c, os) = do
    putStrLn $ "Looking for set of objects less than " ++ show c
    findBest os c

parseError :: String -> IO [Object]
parseError e = putStrLn e >> return []
