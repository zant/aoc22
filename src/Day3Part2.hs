{-# LANGUAGE TemplateHaskell #-}
module Day3 (day3, half,  makeMap, inv) where

import System.Environment (getArgs)
import Prelude
import qualified Data.Map as Map
import Data.Char (ord)

type HMap = Map.Map Char Char

makePair x = (x, x)
half x = splitAt s x where s = length x `div` 2 

makeMap = Map.fromList . map makePair

inv :: Char -> HMap -> (Int, HMap)
inv k m = case Map.lookup k m of
      Just v -> (ascii v, Map.delete k m)
      Nothing -> (0, m)

same :: ([Char], HMap) -> Int -> Int
same (x : xs, m) s = s + same (xs, m') s' where 
    (s', m') = inv x m
same ([], _) s = s

ascii :: Char -> Int
ascii v
  | o > 97 = o `mod` 96
  | o < 97 = (o `mod` 64) + 26
  where o = ord v
ascii _ = 0

day3 = do
  args <- getArgs
  content <- readFile $ head args
  let linesContent = lines content
  let mapIt (x, y)= (x, makeMap y)
  print $ sum $ map (\x -> same ((mapIt . half) x) 0) linesContent