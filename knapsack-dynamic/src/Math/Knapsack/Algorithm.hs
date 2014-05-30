{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import qualified Data.Map as M
import Math.Knapsack.Data
import Pipes
import qualified Pipes.Prelude as P
import System.Hourglass

data KnapsackEntry = KnapsackEntry { objects :: [Object]
                                   , totalValue :: Int
                                   , totalWeight :: Int
                                   } deriving (Eq, Show)

type Coord = (Int, Int)
type KnapsackState = M.Map Coord KnapsackEntry
type WeightLimit = Int
type Knapsack = ReaderT (WeightLimit, [Object]) (StateT KnapsackState IO)

runKnapsack :: Knapsack a -> WeightLimit -> [Object] -> IO (a, KnapsackState)
runKnapsack action lim os = runStateT (runReaderT action (lim, os)) M.empty 

withCoord :: Coord -> Knapsack KnapsackEntry -> Knapsack KnapsackEntry
withCoord coord action = do
    res <- gets $ M.lookup coord
    case res of
        Just val -> return val
        Nothing -> do
            entry <- action
            modify $ M.insert coord entry
            return entry

getObject :: Int -> Knapsack Object
getObject n = asks $ (!! n) . snd

computeObjects :: Int -> Int -> Knapsack KnapsackEntry 
computeObjects 0 _ = return $ KnapsackEntry [] 0 0
computeObjects i j = withCoord (i, j) $ do
    object@(Object _ v c) <- getObject $ pred i
    if c > j then
        computeObjects (pred i) j
    else do
        res1@(KnapsackEntry _ val1 _) <- computeObjects (pred i) j
        (KnapsackEntry os val2 cost2) <- computeObjects (pred i) (j - c)
        if val1 >= (v + val2) then
            return res1
        else
            return $ KnapsackEntry (object : os) (v + val2) (c + cost2)

computeBest :: Knapsack KnapsackEntry
computeBest = do
    (lim, os) <- ask
    computeObjects (length os) lim

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim = liftM (objects . fst) $ runKnapsack computeBest lim os
