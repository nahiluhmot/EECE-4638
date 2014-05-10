-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (subsequences)
import Math.Knapsack.Data
import System.Hourglass
import Data.Hourglass
import Data.Monoid

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
                , endTime :: Elapsed
                } deriving (Eq, Show)

type Knapsack = ReaderT KnapsackEnv (StateT KnapsackState IO)

-- The default state is a list of no objects who have no total value.
defaultState :: KnapsackState
defaultState = KnapsackState { objects = []
                             , totalValue = 0
                             }

-- The length to process subsequences
runTime :: TimeDiff
runTime = mempty { timeDiffMinutes = 10 }

-- Given a Knapsack computation and a limit, return the result and final state.
runKnapsack :: Knapsack a -> Int -> Elapsed -> IO (a, KnapsackState)
runKnapsack action lim startTime = do
    let eTime   = timeAdd startTime runTime
    flip runStateT defaultState . runReaderT action $ KnapsackEnv lim eTime

-- Try each subsequence of the given list.
tryListCombinations :: [Object] -> Knapsack ()
tryListCombinations = mapWithTimeM_ tryList . subsequences

-- Run function across all list elements until the state says to stop
mapWithTimeM_ :: (a -> Knapsack b) -> [a] -> Knapsack ()
mapWithTimeM_ _  []   = return ()
mapWithTimeM_ fn (x:xs) = do
    eTime <- asks endTime
    cTime <- liftIO timeCurrent
    when (cTime <= eTime) $ do
        _ <- fn x
        mapWithTimeM_ fn xs
        return ()

-- Try a list.
tryList :: [Object] -> Knapsack ()
tryList os = do
    lim   <- asks limit
    let size = sum $ map cost os
    when (lim > size) $ do
        val <- gets totalValue
        let currVal = sum $ map value os
        when (currVal > val) $ do
            liftIO $ print ("Found new best value: " ++ show currVal)
            put $ KnapsackState os currVal

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim = do
    print "Starting search"
    start <- timeCurrent
    (_, s) <- runKnapsack (tryListCombinations os) lim start
    return $ objects s
