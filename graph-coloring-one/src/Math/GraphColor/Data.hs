-- | This module holds the 'Edge' data type
module Math.GraphColor.Data ( -- * Data Types
                              Edge(..)
                            , Node(..)
                            ) where

-- | The 'Edge' represents an edge between two nodes of the Graph.
data Edge = Edge { a :: Int
                 , b :: Int
                 } deriving (Eq, Show)

data Node = Node { id :: Int
                 , color :: Int
                 } deriving (Eq, Show)
