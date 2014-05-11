module Math.GraphColor.Algorithm ( findBest
                                 ) where

import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Pipes
import System.Hourglass
import Data.Hourglass


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
subsequences numColors numNodes = do
    let colorList = [1..numColors]
        nodeList  = [0..numNodes]
    each $ nodeLoop colorList nodeList

colorLoop :: Int -> [Int] -> [Node]
colorLoop _ []     = []
colorLoop n (x:xs) = Node n x : colorLoop n xs

nodeLoop :: [Int] -> [Int] -> [[Node]]
nodeLoop _  []      = [[]]
nodeLoop cl (x:xs)  = concatMap singleMap $ colorLoop x cl
    where nl = nodeLoop cl xs
          singleMap z = map (z:) $ nl

checkConflict :: Consumer [Node] GraphColor ()
checkConflict = do
    nodes <- await
    nodeCs <- lift $ forM nodes $ \n -> do
        specificEs <- getNodeEdges n
        currCs <- forM specificEs $ \e -> do
            nextN <- case a e == nodeId n of
                True  -> findNode nodes (b e)
                False -> findNode nodes (a e)
            if color nextN == color n
                then return 1
                else return 0
        return $ sum currCs
    let cs = sum nodeCs
    originalCs <- gets conflicts
    when (originalCs > cs) $ do
        put $ GraphColorState nodes cs
    checkConflict

getNodeEdges :: Node -> GraphColor [Edge]
getNodeEdges node = do
    es <- asks edges
    let n          = nodeId node
        leftEdges  = filter (\x -> a x == n) es
        rightEdges = filter (\x -> b x == n) es
    return $ leftEdges ++ rightEdges

findNode :: [Node] -> Int -> GraphColor Node
findNode ns nId = return $ head $ filter correctN ns
    where correctN n = nodeId n == nId

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
