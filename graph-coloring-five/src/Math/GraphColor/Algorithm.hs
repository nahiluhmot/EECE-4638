{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Math.GraphColor.Algorithm where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Math.GraphColor.Greedy
import System.Hourglass
import Data.Hourglass
import Data.Graph hiding (Node)
import Data.Maybe
import Data.Monoid
import Data.Array
import Data.List
import Pipes
import System.Random

runTime :: TimeDiff
runTime = mempty { timeDiffMinutes = 10 }

generateNeighbors :: Monad m => [(Int, Node)] -> Int -> Producer [(Int, Node)] m ()
generateNeighbors = do
    let go ns  0      = yield ns
        go []  _      = return ()
        go (x:xs) lim = do
            for (go xs (lim - 1)) $ \y -> do
                yield ((fst x, Node Nothing):y)
            for (go xs lim) $ \y -> do
                yield (x:y)
    go

fillInColors :: Int -> Pipe [(Int, Node)] [(Int, Node)] GraphColor ()
fillInColors nc = 
    let go []             = yield []
        go (x@(i,val):xs) = if isJust (color val)
            then for (go xs) $ \ys -> yield (x:ys)
            else for (go xs) $ \ys -> mapM_ (\y -> yield ((i,val { color = Just y }):ys)) [0..nc-1]
    in forever $ do
        elt <- await
        for (go elt) yield

filterTime :: Pipe a a GraphColor ()
filterTime = do
    elt <- await
    et  <- asks endTime
    ct  <- liftIO timeCurrent
    when (ct < et) $ yield elt

validateSolution :: Int -> Consumer [(Int, Node)] GraphColor ()
validateSolution nn = forever $ do
    elt <- await
    g   <- asks envGraph
    gs  <- get
    let nodes = array (0, nn - 1) elt
        cs    = conflicts gs
        newCs = checkConflicts nodes g
    when (newCs <= cs) $ do
        put gs { graphNodes = nodes, conflicts = newCs }

findBestFromNeighbors :: Int -> Int -> Int -> GraphColor ()
findBestFromNeighbors nc nn opt =
    let go cs = do
            gs <- gets graphNodes
            runEffect $ generateNeighbors (assocs gs) opt >-> fillInColors nc >-> validateSolution nn
            newCs <- gets conflicts
            when (newCs < cs) $ go newCs
    in do
        cs <- gets conflicts
        go cs

stdoutLn :: (Monad m, MonadIO m, Show a) => Consumer a m ()
stdoutLn = forever $ do
    elt <- await
    liftIO $ print elt

steepestDescent :: Graph -> Array Int Node -> Int -> Int -> Int -> Int -> IO GraphColorState
steepestDescent g nodes cs nc nn opt = do
    et <- liftIO $ liftM (`timeAdd` runTime) timeCurrent
    (_, newS) <- runGraphColor (e { endTime = et }) s (findBestFromNeighbors nc nn opt)
    return newS
    where e = GraphColorEnv { envGraph = g, envSortedNodes = [], endTime = 0 }
          s = GraphColorState { conflicts = cs, graphNodes = nodes }

run :: Int -> Int -> [Edge] -> IO (Int, [Node])
run nc nn es = do
    randomGen <- getStdGen
    let revEs        = map (\(x,y) -> (y,x)) es
        gbounds      = (0, nn - 1)
        g            = buildG gbounds (es ++ revEs)
        nodes        = listArray gbounds $ repeat (Node Nothing)
        randomColors = take nn $ randomRs (0, nc - 1) randomGen
        randomNodes  = listArray gbounds (map (\x -> (Node $ Just x)) randomColors)
        randomCs     = checkConflicts randomNodes g
        sortedNodes  = map fst $ sortBy (\(_,x) (_,y) -> length y `compare` length x) $ assocs g
    GraphColorState gn cs <- greedySolution g nodes sortedNodes nc nn

    greedyTwo   <- steepestDescent g gn cs nc nn 2
    greedyThree <- steepestDescent g gn cs nc nn 3
    randomTwo   <- steepestDescent g randomNodes randomCs nc nn 2
    randomThree <- steepestDescent g randomNodes randomCs nc nn 3

    let greedyTwoConflicts   = conflicts greedyTwo
        greedyThreeConflicts = conflicts greedyThree
        randomTwoConflicts   = conflicts randomTwo
        randomThreeConflicts = conflicts randomThree
        minConflicts         = minimum [greedyTwoConflicts
                                       , greedyThreeConflicts
                                       , randomTwoConflicts
                                       , randomThreeConflicts
                                       ]

    if minConflicts == randomTwoConflicts then do
        putStrLn "Random solution with 2-opt was the best"
        return (greedyThreeConflicts, elems (graphNodes greedyThree))
    else if minConflicts == randomThreeConflicts then do
        putStrLn "Random solution with 3-opt was the best"
        return (greedyThreeConflicts, elems (graphNodes greedyThree))
    else if minConflicts == greedyTwoConflicts then do
        putStrLn "Greedy solution with 2-opt was the best"
        return (greedyTwoConflicts, elems (graphNodes greedyTwo))
    else do
        putStrLn "Greedy solution with 3-opt was the best"
        return (greedyThreeConflicts, elems (graphNodes greedyThree))
