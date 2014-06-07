{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm where

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
                  , totalCost  :: Int
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
                             , totalCost = 0
                             }

-- The length to process subsequences
runTime :: TimeDiff
runTime = mempty { timeDiffMinutes = 10 }

-- Given a Knapsack computation and a limit, return the result and final state.
runKnapsack :: Knapsack a -> Int -> Elapsed -> IO (a, KnapsackState)
runKnapsack action lim startTime = do
    let eTime = timeAdd startTime runTime
    flip runStateT defaultState . runReaderT action $ KnapsackEnv lim eTime

subsequences :: Monad m => [x] -> Producer [x] m ()
subsequences =
    let go []      = return ()
        go (x:xs) = do
            yield [x]
            for (go xs) $ \subseq -> do
                yield (x : subseq)
                yield subseq
    in  go

bound :: Object -> KnapsackState -> Knapsack (Maybe KnapsackState)
bound o@(Object _ v c) (KnapsackState os v' c') = do
    lim <- asks limit
    tv <- gets totalValue
    let remainingWeight = fromIntegral (lim - c')
        numberToInsert  = remainingWeight / fromIntegral c
        newValue        = v' + round (numberToInsert * fromIntegral v)
    if (newValue < tv) then
        return Nothing
    else
        return $ Just (KnapsackState (o : os) (v + v') (c + c'))


-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os _ = return os
--    start <- timeCurrent
--    (_, s) <- runKnapsack (tryListCombinations os) lim start
--    return $ objects s
