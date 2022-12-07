module Day3Part2 (day3, group, hasCommon, commonMany, execCommonMany, ascii, stringsToAscii) where
import System.Environment (getArgs)
import qualified Data.Map as Map
import Utils (HMap, makeMap)
import Data.Maybe (catMaybes)
import Data.Char (ord)

{-
General hashout

1. A function f that given n strings, finds the common elements in linear time
It does this by creating a map of the first string then
- If match store it and remove it from the map
- If not match go to the next one
- If the second string already doesn't have any elements in common return early (optimization)

A function that groups an array of m strings in n groups

Then we just pass the group to the first function
-}

-- | Base case and inductive
group :: Int -> [a] -> [[a]]
group _ [] = []
group n arr = take n arr : group n (drop n arr)

findInMap :: Ord a => (a, HMap a) -> (Maybe a, HMap a)
findInMap (k, m) = case Map.lookup k m of
  Just v -> (Just v, Map.delete k m)
  Nothing -> (Nothing, m)

hasCommon :: HMap Char -> [Char] -> [Char]
hasCommon _ [] = []
hasCommon m (x : xs) = let (v, m') = findInMap (x, m) in catMaybes [v] ++ hasCommon m' xs

commonMany :: String -> [String] -> [Char]
commonMany m [] = m
commonMany m (x : xs) = commonMany m' xs where m' = hasCommon (makeMap m) x

execCommonMany :: [String] -> [Char]
execCommonMany (x : xs) = commonMany x xs
execCommonMany [] = []

-- | Ascii Manipulation
ascii :: Char -> Int
ascii v = if v' < 97 then v' - 64 + 26 else v' - 96 where v' = ord v

stringsToAscii :: [String] -> [Int]
stringsToAscii = concatMap (map ascii)

day3 :: IO ()
day3 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  let matches = map execCommonMany $ group 3 contentLines
  print $ ("Result is: " ++) . show $ sum $ stringsToAscii matches