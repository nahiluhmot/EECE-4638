module Math.GraphColor.Parser ( parseGraph
                                 , parseFile
                                 ) where

import Control.Monad
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import Math.GraphColor.Data
import System.Directory (doesFileExist)

-- | Parse a natural number (>= 0).
natural :: Parser Int
natural = fmap read $ many1 digit

spaces :: Parser ()
spaces = many' space >> return ()

edge :: Parser Edge
edge = do
    spaces
    left <- natural
    spaces
    right <- natural
    spaces
    return (left, right)

graph :: Parser (Int, Int, Int, [Edge])
graph = do
    spaces
    colors <- natural
    spaces
    numNodes <- natural
    spaces
    numEdges <- natural
    spaces
    edges <- many1 edge
    endOfInput
    return (colors, numNodes, numEdges, edges)

parseGraph :: B.ByteString -> Either String (Int, Int, Int, [Edge])
parseGraph = parseOnly graph

parseFile :: FilePath -> IO (Either String (Int, Int, Int, [Edge]))
parseFile path = do
    exists <- doesFileExist path
    if exists
        then liftM parseGraph $ B.readFile path
        else return . Left $ "No such file: " ++ path
