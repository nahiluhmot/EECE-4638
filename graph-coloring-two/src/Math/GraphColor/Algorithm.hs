{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Math.GraphColor.Algorithm ( findBest
                                 ) where

import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Pipes
import System.Hourglass
import Data.Hourglass
import Data.Graph hiding (Node)
import Data.Monoid
import Data.Array
import Data.Maybe
import Data.List

-- The GraphColorState is a list of nodes with a corresponding color, and the
-- number of conflicts
data GraphColorState =
    GraphColorState { graphNodes    :: Array Int Node
                    , conflicts     :: Int
                    }
                    deriving (Eq, Show)

-- The environment is the number of colors available, the number of nodes, and
-- the corresponding edges between nodes
data GraphColorEnv =
    GraphColorEnv { envGraph       :: Graph
                  , envSortedNodes :: [Vertex]
                  , endTime        :: Elapsed
                  } deriving (Eq, Show)

type GraphColor = ReaderT GraphColorEnv (StateT GraphColorState IO)

runTime :: TimeDiff
runTime = mempty { timeDiffMinutes = 10 }

runGraphColor :: Graph -> Array Int Node -> [Vertex] -> Elapsed -> GraphColor a -> IO (a, GraphColorState)
runGraphColor g nodes sortedNodes et action =
    runStateT (runReaderT action e) s
    where e = GraphColorEnv { envGraph = g, envSortedNodes = sortedNodes, endTime =  et }
          s = GraphColorState { conflicts = 0, graphNodes = nodes }

greedyColors :: Int -> Int -> GraphColor ()
greedyColors nc nn = runEffect $
    allowableConflicts >-> pushColors nc >-> vertexes >-> checkConflicts >-> finishedColors nn >-> consume

allowableConflicts :: Producer Int GraphColor ()
allowableConflicts = do
    let loop x = yield x >> loop (x + 1)
    loop 0

pushColors :: Int -> Pipe Int (Int, Int) GraphColor ()
pushColors nc = do
    cs <- await
    for (each [0..nc-1]) $ \x -> yield (cs, x)
    pushColors nc

vertexes :: Pipe (Int, Int) Vertex GraphColor ()
vertexes = do
    (allowedCs, c) <- await
    liftIO $ putStrLn $ "Color: " ++ show c
    sNodes <- asks envSortedNodes
    g      <- asks envGraph
    for (each sNodes) $ \nVertex -> do
        gs <- get
        ns <- gets graphNodes
        let n    = ns ! nVertex
            es   = g ! nVertex
            ens  = map (ns !) es
            cens = mapMaybe color ens
            newN = n { color = Just c }
            conflict = length (filter (c==) cens)
            hasColor = isJust (color n) 
        when (not hasColor && conflict < allowedCs) $ do
            let newNs = ns // [(nVertex, newN)]
            put gs { graphNodes = newNs }
            yield nVertex
    vertexes

checkConflicts :: Pipe a a GraphColor ()
checkConflicts = do
    elt <- await
    gs <- get
    ns <- gets graphNodes
    g <- asks envGraph
    let nConflicts = map check (assocs g)
        c = color . (ns !)
        ens = map (ns !)
        cens = mapMaybe color . ens
        getConflicts (i, es) = length (filter (fromJust (c i) ==) (cens es))
        check (i, es) = if isJust (c i) then getConflicts (i, es) else -1
        cs = sum nConflicts `div` 2
    put (gs { conflicts = cs })
    yield elt
    checkConflicts

finishedColors :: Int -> Pipe a a GraphColor ()
finishedColors nc = do
    elt <- await
    ns <- gets graphNodes
    let cs = nc - length (mapMaybe color (elems ns))
    when (cs > 0) $ do
        yield elt
        liftIO $ putStrLn $ "Missing colors: " ++ show cs
        finishedColors nc

consume :: Consumer Vertex GraphColor ()
consume = do
    vertex <- await
    liftIO $ putStrLn $ "Changed: " ++ show vertex
    consume

findBest :: Int -> Int -> [Edge] -> IO (Int, [Node])
findBest nc nn es = do
    currentT <- timeCurrent
    let endT        = timeAdd currentT runTime
    let revEs       = map (\(x,y) -> (y,x)) es
        gbounds     = (0, nn - 1)
        g           = buildG gbounds (es ++ revEs)
        nodes       = listArray gbounds $ repeat (Node Nothing)
        sortedNodes = map fst $ sortBy (\(_,x) (_,y) -> length y `compare` length x) $ assocs g
    (_, s) <- runGraphColor g nodes sortedNodes endT (greedyColors nc nn)
    return (conflicts s, elems (graphNodes s))
