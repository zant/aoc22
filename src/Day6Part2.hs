module Day6Part2 (day6) where
import Day6 (findDuplicates)
import Data.Containers.ListUtils (nubOrd)
import System.Environment (getArgs)

day6 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ findDuplicates 0 14 $ head contentLines
