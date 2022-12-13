module Day5Part2 (day5, transition, calculate) where
import System.Environment (getArgs)
import Data.Either (rights)
import Day5 (Move, movesParser, regularParse, replace, parseMoves, elems)

-- Remove the reverse from part 1
transition :: [[Char]] -> Move -> [[Char]]
transition e (a,b,c) = replace (replace e (b-1) x) (c-1) f where 
                                          (x,y) = splitAt (length b' - a) b'
                                          f = c' ++ y
                                          b' = e!!(b-1)
                                          c' = e!!(c-1)

calculate a b = map last $ foldl transition b moves where moves = rights $ map parseMoves a

day5 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ calculate contentLines elems
  