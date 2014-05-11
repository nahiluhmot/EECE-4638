module Math.GraphColor.Algorithm ( findBest
                                 ) where

import Control.Monad.Reader
import Control.Monad.State
import Math.GraphColor.Data
import Pipes
import System.Hourglass
import Data.Hourglass
import Data.Monoid


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
                  , endTime   :: Elapsed
                  } deriving (Eq, Show)

type GraphColor = ReaderT GraphColorEnv (StateT GraphColorState IO)

defaultState :: GraphColorState
defaultState = GraphColorState { nodeColors = []
                               , conflicts  = maxBound
                               }

runTime :: TimeDiff
runTime = mempty { timeDiffMinutes = 10 }

runGraphColor :: [Edge] -> Elapsed -> GraphColor a -> IO (a, GraphColorState)
runGraphColor es et action = flip runStateT defaultState . runReaderT action $ GraphColorEnv es et

tryListCombinations :: Int -> Int -> GraphColor ()
tryListCombinations nc nn = runEffect $
    subsequences nc nn >-> filterTime >-> consume

subsequences :: Int -> Int -> Producer [Node] GraphColor ()
subsequences cs ns =
    let loop n =
            if n == ns
            then yield []
            else for (colorLoop n cs) $ \node ->
                    for (loop (n + 1)) $ \nodes ->
                        yield $ node : nodes
    in  loop 0

colorLoop :: Monad m => Int -> Int -> Producer Node m ()
colorLoop n maxColor =
    let loop c =
            unless (c > maxColor) $ do
                yield $ Node n c
                loop (c + 1)
    in loop 1

consume :: Consumer [Node] GraphColor ()
consume = do
    es <- asks edges
    nodes <- await
    unless (null nodes || (color (head nodes) > 1)) $ do
        originalCs <- gets conflicts
        let checkConflicts' acc e = acc + fromEnum (color left == color right)
                where left  = nodes !! a e
                      right = nodes !! b e
            checkConflicts acc []     = acc
            checkConflicts acc (e:es') = if acc > originalCs
                                        then acc
                                        else checkConflicts (checkConflicts' acc e) es'
        let cs = checkConflicts 0 es
        when (originalCs > cs) . put $ GraphColorState nodes cs
        unless (cs == 0) consume

-- Don't compute the computation run if time has run out.
filterTime :: Pipe a a GraphColor ()
filterTime =
    let loop eTime = do
            elt <- await
            cTime <- liftIO timeCurrent
            when (eTime > cTime) $ yield elt >> loop eTime
    in  asks endTime >>= loop

findBest :: Int -> Int -> [Edge] -> IO (Int, [Node])
findBest nc nn es = do
    currentT <- timeCurrent
    let endT = timeAdd currentT runTime
    (_, s) <- runGraphColor es endT (tryListCombinations nc nn)
    return (conflicts s, nodeColors s)
