-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (subsequences)
import Math.Knapsack.Data

-- The KnapsackState is a list of objects, along with a cached version of their
-- combined value.
data KnapsackState =
    KnapsackState { objects    :: [Object]
                  , totalValue :: Int
                  }
                  deriving (Eq, Show)

-- Currently, the environment is just the weight limit of the Knapsack. A time
-- limit should also be added to this, so that it will stop working after that time.
data KnapsackEnv =
    KnapsackEnv { limit :: Int
                } deriving (Eq, Show)

type Knapsack = ReaderT KnapsackEnv (StateT KnapsackState IO)

-- The default state is a list of no objects who have no total value.
defaultState :: KnapsackState
defaultState = KnapsackState { objects = [], totalValue = 0 }

-- Given a Knapsack computation and a limit, return the result and final state.
runKnapsack :: Knapsack a -> Int -> IO (a, KnapsackState)
runKnapsack action lim =
    flip runStateT defaultState . runReaderT action $ KnapsackEnv lim

-- Try each subsequence of the given list.
tryListCombinations :: [Object] -> Knapsack ()
tryListCombinations = mapM_ tryList . reverse . subsequences

-- Try a list.
tryList :: [Object] -> Knapsack ()
tryList os = do
    lim <- asks limit
    let size = foldr (+) 0 $ map cost os
    when (lim > size) $ do
        val <- gets totalValue
        let currVal = foldr (+) 0 $ map value os
        when (currVal > val) . put $ KnapsackState os currVal 

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim =
    liftM (objects . snd) $ runKnapsack (tryListCombinations os) lim
