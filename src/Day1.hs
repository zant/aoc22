module Day1
    ( someFunc
    ) where

import           Data.List                      ( minimumBy )
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

shouldInclude :: [Char] -> [Integer] -> [Integer]
shouldInclude x y = if a == 0 then 0 : y else a + head y : tail y
    where a = readSafe x

parseBreak :: [[Char]] -> [Integer]
parseBreak = foldr shouldInclude [0]

readSafe :: String -> Integer
readSafe = fromMaybe 0 . readMaybe

someFunc :: IO ()
someFunc = do
    args    <- getArgs
    content <- readFile $ head args
    let linesOfLines = lines content
    let elems        = parseBreak linesOfLines
    print $ minimumBy (flip compare) elems
