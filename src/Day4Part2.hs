module Day4Part2 (day4) where
import Day4 (parsePairs, regularParse)
import System.Environment (getArgs)
import Data.Either (rights)

inRange :: ((Int, Int), (Int, Int)) -> Int
inRange ((a,b), (c,d)) = if c - b > 0 || a - d > 0 then 0 else 1

day4 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ (sum . rights) $ map (fmap inRange . regularParse parsePairs) contentLines

