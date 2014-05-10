{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm (findBest) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Hourglass
import Data.Monoid
import Math.Knapsack.Data
import Pipes
import qualified Pipes.Prelude as P
import System.Hourglass

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
    KnapsackEnv { limit   :: Int
                , endTime :: Elapsed
                } deriving (Eq, Show)

-- This is the Monad that will be used throughout the computations.
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
    let eTime = timeAdd startTime runTime
    flip runStateT defaultState . runReaderT action $ KnapsackEnv lim eTime

-- Try each subsequence of the given list.
tryListCombinations :: [Object] -> Knapsack ()
tryListCombinations objs = runEffect $
    subsequences objs >-> filterHeavy >-> filterTime >-> P.mapM tryList >-> P.drain

-- Produce the subsequences of a list.
subsequences :: Monad m => [a] -> Producer [a] m ()
subsequences = 
    let loop [] = return ()
        loop (x:xs) = do
            yield [x]
            for (loop xs) $ \sub -> do
                yield sub
                yield $ x : sub
    in  loop

-- | Filter out the objects that are too heavy.
filterHeavy :: Pipe [Object] [Object] Knapsack ()
filterHeavy = asks limit >>= \lim -> P.filter $ \objs -> lim > sum (map cost objs)

-- Don't compute the computation run if time has run out.
filterTime :: Pipe a a Knapsack ()
filterTime =
    let loop eTime = do
            elt <- await
            cTime <- liftIO timeCurrent
            when (eTime > cTime) $ yield elt >> loop eTime
    in  asks endTime >>= loop

-- Try a list.
tryList :: [Object] -> Knapsack ()
tryList os = do
    val <- gets totalValue
    let currVal = sum $ map value os
    when (currVal > val) . put $ KnapsackState os currVal

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim = do
    start <- timeCurrent
    (_, s) <- runKnapsack (tryListCombinations os) lim start
    return $ objects s
