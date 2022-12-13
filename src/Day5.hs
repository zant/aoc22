module Day5 (day5, movesParser, regularParse, replace, transition, calculate, Move, parseMoves, elems) where
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Either (rights)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "day6"

movesParser :: Parser Move
movesParser = do
  string "move"
  space
  a <- many1 digit
  space
  string "from"
  space
  b <- many1 digit
  space
  string "to"
  space
  c <- many1 digit
  return (read a,read b,read c)

parseMoves = regularParse movesParser

type Move = (Int, Int, Int)
elems = ["NBDTVGZJ",
        "SRMDWPF",
        "VCRSZ",
        "RTJZPHG",        
        "TCJNDZQF",        
        "NVPWGSFM",        
        "GCVBPQ",        
        "ZBPN",        
        "WPJ"]

replace :: [a] -> Int -> a -> [a]
replace a i v = x ++ v : tail y where (x,y) = splitAt i a

transition :: [[Char]] -> Move -> [[Char]]
transition e (a,b,c) = replace (replace e (b-1) x) (c-1) f where 
                                          (x,y) = splitAt (length b' - a) b'
                                          f = c' ++ reverse y
                                          b' = e!!(b-1)
                                          c' = e!!(c-1)

calculate a b = map last $ foldl transition b moves where moves = rights $ map parseMoves a

day5 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ calculate contentLines elems
  