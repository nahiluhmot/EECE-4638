-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Arrow (first, second)
import Control.Monad.Reader
import Data.Hourglass
import Data.Monoid
import Data.List (sortBy, permutations)
import Data.Function (on)
import Math.Knapsack.Data
import System.Hourglass
import Pipes
import qualified Pipes.Prelude as P

-- This is the greedy solution to the knapsack problem
greedySolution :: Int -> [Object] -> ([Object], [Object])
greedySolution lim = takeMaximum  lim . sort'

-- Shuffle a list of objects
shuffle :: Int -> [Object] -> ([Object], [Object])
shuffle lim os = takeMaximum lim $ permutations os !! (length os * length os)

-- Sort a list of objects by their value density
sort' :: [Object] -> [Object]
sort' = sortBy (compare `on` valuePerCost)

-- Find all of the subsequences
subsequences' :: Monad m => [a] -> Producer ([a], [a]) m ()
subsequences' =
    let go [] = yield ([], [])
        go (x:xs) =
            for (go xs) $ \(ys, zs) -> do
                yield (ys, x : zs)
                yield (x : ys, zs)
    in  go

-- Find all of the neighbors
neighbors :: Monad m => Int -> Int -> ([Object], [Object]) -> Producer ([Object], [Object]) m ()
neighbors lim n (inKnapsack, outKnapsack) = do
    let inKnapsack' = drop n $ sort' inKnapsack
    for (subsequences' outKnapsack) $ \(nextIn, nextOut) -> do
        let (inGroup, outGroup) = takeMaximum lim . sort' $ inKnapsack' ++ nextIn
        yield (inGroup, outGroup ++ nextOut)

-- Find the best neighbor within the given bounds
bestNeighbor :: Monad m => Int -> Int -> ([Object], [Object]) -> m ([Object], [Object])
bestNeighbor lim n os =
    let rightSize (xs, _) = sum (map cost xs) <= lim
        takeBest best@(v, _, _) (ys, zs)
            | tv > v = (tv, ys, zs)
            | otherwise = best
            where tv = sum $ map value ys
     in  liftM (\(_, a, b) -> (a, b)) $
             P.fold takeBest
                    (0, [], [])
                    Prelude.id
                    (neighbors lim n os >-> P.filter rightSize)

-- Given the limit and list of objects, return a tuple containing the objects
-- in the knapsack, and the objects not in the knapsack
takeMaximum :: Int -> [Object] -> ([Object], [Object])
takeMaximum limit =
    let insertIf lim (t, os, os') o@(Object _ _ c)
            | t + c < lim = (t + c, o : os, os')
            |   otherwise = (t, os, o : os')
    in  (\(_, a, b) -> (a, b)) . foldl (insertIf limit) (0, [], [])

-- Given a limit, number of "opts" a list of objects in the knapsack, and a
-- list of objects out of the knapsack, try to find the best solution.
steepestDescent :: Int -> Int -> ([Object], [Object]) -> IO [Object]
steepestDescent limit n os' =
    let go os endTime = do
            now <- timeCurrent
            if now > endTime then
                return $ fst os
            else do
                neighbor <- bestNeighbor limit n os
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
