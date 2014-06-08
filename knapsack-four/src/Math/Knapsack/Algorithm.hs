{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | This module holds the actual algorithm to solve the Knapsack problem.
module Math.Knapsack.Algorithm where

import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Data.Hourglass
import Data.Monoid
import qualified Data.Foldable as F
import Data.List (sortBy)
import Math.Knapsack.Data as D
import Pipes
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

tryListCombinations :: [Object] -> Knapsack ()
tryListCombinations os = runEffect $ subsequences os' >-> filterTime >-> boundCompare
    where os' = sortBy (\a b -> valuePerCost a `compare` (valuePerCost b :: Double)) os

subsequences :: [Object] -> Producer KnapsackState Knapsack ()
subsequences =
    let yieldKnapsack os = do
            let (o, ks) = knapsackFor os
            ks' <- lift $ bound o ks
            F.mapM_ yield ks'
        go []      = return ()
        go (x:xs) = do
            for (go xs) $ \subseq -> do
                let os = objects subseq
                yieldKnapsack (x : os)
                yieldKnapsack os
            yieldKnapsack [x]
    in  go

knapsackFor :: [Object] -> (Object, KnapsackState)
knapsackFor os = (last os, KnapsackState os' v c)
    where os' = init os
          v   = sum $ map value os'
          c   = sum $ map cost os'

bound :: Object -> KnapsackState -> Knapsack (Maybe KnapsackState)
bound o@(Object _ v c) (KnapsackState os v' c') = do
    lim <- asks limit
    tv <- gets totalValue
    let remainingWeight = fromIntegral (lim - c')
        numberToInsert  = remainingWeight / fromIntegral c
        newValue        = v' + ceiling (numberToInsert * fromIntegral v :: Double)
    return $ if newValue <= tv || c' + c > lim
        then Nothing
        else Just (KnapsackState (o : os) (v + v') (c + c'))

-- Don't compute the computation run if time has run out.
filterTime :: Pipe a a Knapsack ()
filterTime =
    let loop eTime = do
            elt <- await
            cTime <- liftIO timeCurrent
            when (eTime > cTime) $ yield elt >> loop eTime
    in  asks endTime >>= loop

boundCompare :: Consumer KnapsackState Knapsack ()
boundCompare = do
    ks <- await
    tv <- gets totalValue
    when (totalValue ks > tv) $ put ks
    boundCompare

-- | Given a list, try to find the subsequence that will give the most value
-- when put into the Knapsack.
findBest :: [Object] -> Int -> IO [Object]
findBest os lim = do
    start <- timeCurrent
    (_, s) <- runKnapsack (tryListCombinations os) lim start
    --return $ objects s
    return . sortBy (compare `on` D.id) $ objects s
