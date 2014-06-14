-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (subsequences, findBest) where

import Control.Monad.Reader
import Data.Hourglass
import Data.Monoid
import Data.List (sortBy, permutations)
import Data.Function (on)
import Math.Knapsack.Data
import System.Hourglass

greedySolution :: Int -> [Object] -> ([Object], [Object])
greedySolution lim = takeMaximum  lim . sort'

shuffle :: Int -> [Object] -> ([Object], [Object])
shuffle lim os = takeMaximum lim $ permutations os !! (length os * length os)

sort' :: [Object] -> [Object]
sort' = sortBy (compare `on` valuePerCost)

-- Given the limit and list of objects, return a tuple containing the objects
-- in the knapsack, and the objects not in the knapsack
takeMaximum :: Int -> [Object] -> ([Object], [Object])
takeMaximum limit =
    let insertIf lim (t, os, os') o@(Object _ _ c)
            | t + c < lim = (t + c, o : os, os')
            |   otherwise = (t, os, o : os')
    in  (\(_, a, b) -> (a, b)) . foldl (insertIf limit) (0, [], [])

neighbors :: Int -> Int -> ([Object], [Object]) -> [([Object], [Object])]
neighbors lim n (inKnapsack, outKnapsack) =
    let inKnapsack' = drop n $ sort' inKnapsack
    in  map (\(inGroup, outGroup) ->
                let (inGroup', outGroup') = takeMaximum lim $ sort' inGroup
                in  (inGroup', outGroup' ++ outGroup)) $
            map (\(inGroup, outGroup) -> (inGroup ++ inKnapsack', outGroup))
                (subsequences outKnapsack)

subsequences :: [a] -> [([a], [a])]
subsequences =
    let go [] = []
        go [x] = [([x], [])]
        go (x:xs) = ([x], xs) :
                    map (\(ys, zs) -> (ys, x : zs)) (go xs) ++
                    map (\(ys, zs) -> (x : ys, zs)) (go xs)
    in  go

bestNeighbor :: Int -> Int -> ([Object], [Object]) -> ([Object], [Object])
bestNeighbor lim i o =
    let k neighbor (os, n)
            | tc > lim  = (os, n)
            | tv < n    = (os, n)
            | otherwise = (neighbor, tv)
            where tc = sum . map cost . fst $ neighbor
                  tv = sum . map value . fst $ neighbor
    in  fst . foldr k (([], []), 0) $ neighbors lim i o

steepestDescent :: Int -> Int -> ([Object], [Object]) -> IO [Object]
steepestDescent limit n os' =
    let go os endTime = do
            now <- timeCurrent
            if now > endTime then
                return $ fst os
            else do
                let neighbor = bestNeighbor limit n os
                if sum (map value (fst neighbor)) < sum (map value (fst os)) then
                    return $ fst os
                else
                    go neighbor endTime
    in  liftM (`timeAdd` mempty { timeDiffMinutes = 10 }) timeCurrent >>= go os'

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim = do
    greedy2 <- steepestDescent lim 2 (greedySolution lim os)
    greedy3 <- steepestDescent lim 3 (greedySolution lim os)
    random2 <- steepestDescent lim 2 (shuffle lim os)
    random3 <- steepestDescent lim 3 (shuffle lim os)

    let greedy2Sol = totalValue greedy2
        greedy3Sol = totalValue greedy3
        random2Sol = totalValue random2
        random3Sol = totalValue random3
        best       = maximum [greedy2Sol, greedy3Sol, random2Sol, random3Sol]
        totalValue = sum . map value

    if best == greedy2Sol then do
        putStrLn "Greedy 2-opt was the best"
        return greedy2
    else if best == greedy3Sol then do
        putStrLn "Greedy 3-opt was the best"
        return greedy3
    else if best == greedy3Sol then do
        putStrLn "Random 2-opt was the best"
        return random2
    else do
        putStrLn "Random 3-opt was the best"
        return random3
