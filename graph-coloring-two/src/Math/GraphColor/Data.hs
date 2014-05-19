{-# LANGUAGE OverlappingInstances,
             FlexibleInstances #-}

-- | This module holds the 'Edge' data type
module Math.GraphColor.Data ( -- * Data Types
                              Edge
                            , Node(..)
                            ) where

import Data.Graph

-- | The 'Edge' represents an edge between two nodes of the Graph.
--data Edge = Edge { a :: Int
--                 , b :: Int
--                 } deriving (Eq, Show)

data Node = Node { color :: Maybe Int
                 } deriving (Eq, Show)

instance Show [Node] where
    show = showNodes 0

showNodes :: Int -> [Node] -> String
showNodes _     []     = "Empty nodes"
showNodes index (x:[]) = showNode index x
showNodes index (x:xs) = showNode index x ++ "\n" ++ showNodes (index + 1) xs

showNode :: Int -> Node -> String
showNode i x = show i ++ "\t" ++ show (color x)
