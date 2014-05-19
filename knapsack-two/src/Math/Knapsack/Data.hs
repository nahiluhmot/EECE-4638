-- | This module holds the 'Object' data type, along with some utility functions
-- for it.
module Math.Knapsack.Data ( -- * Data Types
                            Object(..)
                            -- * Utility Functions
                          , valuePerCost
                          ) where

-- | The 'Object' represents an object that could be put into the Knapsack.
data Object = Object { id    :: Int -- ^ A unique identifier for the 'Object'.
                     , value :: Int -- ^ The value of the 'Object'.
                     , cost  :: Int -- ^ The cost of the 'Object'.
                     } deriving (Eq) 

-- | Compute the weighted value of the given 'Object'.
valuePerCost :: Object -> Double
valuePerCost (Object _ v c) = fromIntegral v / fromIntegral c

instance Show Object where
    show (Object i v c) = show i ++ " " ++ show v ++ " " ++ show c
