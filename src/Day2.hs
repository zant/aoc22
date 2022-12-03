module Day2 (day2, parseLetter, parseLetters, Move (..), parsePair) where
import           System.Environment             ( getArgs )
import           Data.Maybe                     ( fromMaybe )

data Move v = Rock !Int | Paper !Int | Scissors !Int | Invalid
  deriving (Eq, Show)


parseLetter :: Char -> Maybe (Move Int)
parseLetter letter = case letter of
  'A' -> Just (Rock 1)
  'B' -> Just (Paper 2)
  'C' -> Just (Scissors 3)
  'X' -> Just (Rock 1)
  'Y' -> Just (Paper 2)
  'Z' -> Just (Scissors 3)
  _   -> Nothing

parseFrom = fromMaybe Invalid . parseLetter

parsePair (x, y) = case (x, y) of
    (Paper    _, Paper v   ) -> 3 + v
    (Paper    _, Scissors v) -> 6 + v
    (Paper    _, Rock v    ) -> 0 + v
    (Rock     _, Rock v    ) -> 3 + v
    (Rock     _, Paper v   ) -> 6 + v
    (Rock     _, Scissors v) -> 0 + v
    (Scissors _, Scissors v) -> 3 + v
    (Scissors _, Rock v    ) -> 6 + v
    (Scissors _, Paper v   ) -> 0 + v
    (_, _) -> 0
-- string of the form "A B"
parseLetters (x : _ : y : ys) =
  parseLetters ys +parsePair (parseFrom x, parseFrom y)
parseLetters _ = 0


day2 :: IO ()
day2 = do
  args    <- getArgs
  content <- readFile $ head args
  let linesOfLines = lines content
  print $ sum $ map parseLetters linesOfLines
