{-# LANGUAGE OverlappingInstances,
             FlexibleInstances #-}

-- | This module holds the 'Edge' data type
module Math.GraphColor.Data ( -- * Data Types
                              Edge
                            , Node(..)
                            , GraphColorEnv(..)
                            , GraphColorState(..)
                            , GraphColor
                            , runGraphColor
                            , checkConflicts
                            ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Graph
import Data.Hourglass
import Data.Maybe

data Node = Node { color :: Maybe Int
                 } deriving (Eq, Show)

instance Show [Node] where
    show = showNodes 0

showNodes :: Int -> [Node] -> String
showNodes _     [] = "Empty nodes"
showNodes i (x:[]) = showNode i x
showNodes i (x:xs) = showNode i x ++ "\n" ++ showNodes (i + 1) xs

showNode :: Int -> Node -> String
showNode i x = show i ++ "\t" ++ c (color x)
    where c (Just y) = show y
          c _        = "Nothing"

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

runGraphColor :: GraphColorEnv -> GraphColorState -> GraphColor a -> IO (a, GraphColorState)
runGraphColor e s action = runStateT (runReaderT action e) s

checkConflicts :: Array Int Node -> Graph -> Int
checkConflicts ns g = 
    let c = color . (ns !)
        ens = map (ns !)
        cens = mapMaybe color . ens
        getConflicts (i, es) = length (filter (fromJust (c i) ==) (cens es))
        check (i, es) = if isJust (c i) then getConflicts (i, es) else -1
        nConflicts = map check (assocs g)
        cs = sum nConflicts `div` 2
    in cs
