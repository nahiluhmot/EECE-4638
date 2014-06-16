{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Math.GraphColor.Greedy (greedySolution) where

import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Pipes
import Data.Graph hiding (Node)
import Data.Array
import Data.Maybe

greedySolution :: Graph -> Array Int Node -> [Vertex] -> Int -> Int -> IO GraphColorState
greedySolution g nodes sortedNodes nc nn = do
    (_, newS) <- runGraphColor e s (greedyColors nc nn)
    return newS
    where e = GraphColorEnv { envGraph = g, envSortedNodes = sortedNodes, endTime =  0 }
          s = GraphColorState { conflicts = 0, graphNodes = nodes }

greedyColors :: Int -> Int -> GraphColor ()
greedyColors nc nn = runEffect $
    allowableConflicts >-> pushColors nc >-> vertexes >-> finishedColors nn

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
    sNodes <- asks envSortedNodes
    g      <- asks envGraph
    for (each sNodes) $ \nVertex -> do
        gs <- get
        ns <- gets graphNodes
        let n    = ns ! nVertex
            es   = g  ! nVertex
            ens  = map (ns !) es
            cens = mapMaybe color ens
            newN = n { color = Just c }
            conflict = length (filter (c==) cens)
            hasColor = isJust (color n) 
        when (not hasColor && conflict < allowedCs) $ do
            let newNs = ns // [(nVertex, newN)]
                cs = checkConflicts newNs g
            put gs { graphNodes = newNs, conflicts = cs }
            yield nVertex
    vertexes

finishedColors :: Int -> Consumer a GraphColor ()
finishedColors nc = do
    await
    ns <- gets graphNodes
    let cs = nc - length (mapMaybe color (elems ns))
    when (cs > 0) $ finishedColors nc
