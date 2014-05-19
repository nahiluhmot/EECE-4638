-- | This module handles the parsing for the application. The format that is
-- parsed is defined in 'knapsack.format'.
module Math.Knapsack.Parser ( parseKnapsack
                            , parseFile
                            ) where

import Control.Monad
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import Math.Knapsack.Data
import System.Directory (doesFileExist)

-- | Parse a natural number (>= 0).
natural :: Parser Int
natural = fmap read $ many1 digit

spaces :: Parser ()
spaces = many' space >> return ()

-- | Parse an 'Object'.
object :: Parser Object
object = do
    i <- natural
    spaces
    v <- natural
    spaces
    c <- natural
    spaces
    return $ Object i v c

-- | Parse the format defined in "knapsack.format".
knapsack :: Parser (Int, Int, [Object])
knapsack = do
    spaces
    total <- natural
    spaces
    size <- natural
    spaces
    objects <- many1 object
    endOfInput

    return (total, size, objects)

-- | Parse the given 'B.ByteString'.
parseKnapsack :: B.ByteString -> Either String (Int, Int, [Object])
parseKnapsack = parseOnly knapsack

-- | Try to run 'parseKnapsack' given a 'FilePath'.
parseFile :: FilePath -> IO (Either String (Int, Int, [Object]))
parseFile path = do
    exists <- doesFileExist path
    if exists
        then liftM parseKnapsack $ B.readFile path
        else return . Left $ "No such file: " ++ path
