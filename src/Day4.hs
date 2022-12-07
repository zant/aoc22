module Day4 (day4) where
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Either (rights)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseRange :: Parser (Int, Int)
parseRange = do
  m <- many1 digit 
  char '-'
  n <- many1 digit
  return (read m, read n)

parsePairs :: Parser ((Int, Int), (Int, Int))
parsePairs = do
  m <- parseRange
  char ','
  n <- parseRange
  return (m, n)
 
fullyContained :: ((Int, Int), (Int, Int)) -> Int
fullyContained ((a,b), (c,d)) = if min (c - a) (b - d) >= 0 || min (a - c) (d - b) >= 0 then 1 else 0

day4 = do
  args <- getArgs
  contentLines <- lines <$> readFile (head args)
  print $ (sum . rights) $ map (fmap fullyContained . regularParse parsePairs) contentLines

