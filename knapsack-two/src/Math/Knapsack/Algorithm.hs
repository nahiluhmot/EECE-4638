-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (sortBy)
import Math.Knapsack.Data

-- This is the Monad that will be used throughout the computations.
type Knapsack = ReaderT Int (State ([Object], Int))

-- Given a Knapsack computation and a limit, return the result and final state.
execKnapsack :: Knapsack a -> Int -> ([Object], Int)
execKnapsack action = flip execState ([], 0) . runReaderT action

-- Given a list of objects, try to fit them into the Knapsack.
tryObjects :: [Object] -> Knapsack ()
tryObjects =
    let sortFn a b = valuePerCost b `compare` valuePerCost a
    in  mapM_ tryObject . reverse . sortBy sortFn

-- Given an object, try to fit it into the Knapsack.
tryObject :: Object -> Knapsack ()
tryObject o@(Object _ _ weight) = do
    lim <- ask
    (os, totalWeight) <- get
    when ((weight + totalWeight) <= lim) $ put (o : os, weight + totalWeight)

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> [Object]
findBest os = fst . execKnapsack (tryObjects os)
