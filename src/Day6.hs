module Day6 (day6, hasDuplicate, findDuplicates) where
import Data.Containers.ListUtils (nubOrd)
import System.Environment (getArgs)

hasDuplicate a = a /= nubOrd a

findDuplicates i n a = if hasDuplicate a' && i <= length a then findDuplicates (i + 1) n a else i + n where a' = take n $ drop i a

day6 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ findDuplicates 0 4 $ head contentLines
