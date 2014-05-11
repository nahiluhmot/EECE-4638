module Math.GraphColor.Algorithm ( findBest
                                 ) where

import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Pipes
--import System.Hourglass
--import Data.Hourglass


-- The GraphColorState is a list of nodes with a corresponding color, and the
-- number of conflicts
data GraphColorState =
    GraphColorState { nodeColors :: [Node]
                    , conflicts  :: Int
                    }
                    deriving (Eq, Show)

-- The environment is the number of colors available, the number of nodes, and
-- the corresponding edges between nodes
data GraphColorEnv =
    GraphColorEnv { edges     :: [Edge]
                  } deriving (Eq, Show)

type GraphColor = ReaderT GraphColorEnv (StateT GraphColorState IO)

defaultState :: GraphColorState
defaultState = GraphColorState { nodeColors = []
                               , conflicts  = maxBound
                               }

runGraphColor :: [Edge] -> GraphColor a -> IO (a, GraphColorState)
runGraphColor es action = flip runStateT defaultState . runReaderT action $ GraphColorEnv es

subsequences :: Int -> Int -> Producer [Node] GraphColor ()
subsequences numColors numNodes =
    let colorList = [1..numColors]
        nodeList  = [0..numNodes]
    in  nodeLoop colorList nodeList

colorLoop :: Monad m => Int -> [Int] -> Producer Node m ()
colorLoop n cs = for (each cs) $ yield . Node n

nodeLoop :: Monad m => [Int] -> [Int] -> Producer [Node] m ()
nodeLoop cs =
    let loop [] = yield []
        loop (n:ns) =
            for (colorLoop n cs) $ \node ->
                for (loop ns) $ \nodes ->
                    yield $ node : nodes
    in  loop

checkConflict :: Consumer [Node] GraphColor ()
checkConflict = do
    es <- asks edges
    nodes <- await
    unless (null nodes || (color (head nodes) > 1)) $ do
        edgeCs <- lift $ forM es $ \e -> do
            let left = nodes !! a e
                right = nodes !! b e
            return . fromEnum $ color left == color right
        let cs = sum edgeCs
        originalCs <- gets conflicts
        when (originalCs > cs) . put $ GraphColorState nodes cs
        unless (cs == 0) checkConflict

tryListCombinations :: Int -> Int -> GraphColor ()
tryListCombinations nc nn = runEffect $
    subsequences nc nn >-> checkConflict

findBest :: Int -> Int -> [Edge] -> IO [Node]
findBest nc nn es = do
    putStrLn "Starting to build combinations"
    (_, s) <- runGraphColor es (tryListCombinations nc nn)
    putStrLn $ "Finished running, only " ++ show (conflicts s) ++ " conflicts"
    putStrLn $ "Colors: " ++ show (nodeColors s)
    return $ nodeColors s
