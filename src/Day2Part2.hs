module Day2Part2 (day2Part2, parseLetter, parseLetters, Move (..)) where
import           System.Environment             ( getArgs )
import           Data.Maybe                     ( fromMaybe )
import Day2 (parsePair, Move(..))

data State = Lose | Draw | Win | None
  deriving (Eq, Show)

parseLetter :: Char -> Maybe (Move Int)
parseLetter letter = case letter of
  'A' -> Just (Rock 1)
  'B' -> Just (Paper 2)
  'C' -> Just (Scissors 3)
  _ -> Nothing


parseState :: Char -> Maybe State
parseState letter = case letter of
  'X' -> Just Lose
  'Y' -> Just Draw
  'Z' -> Just Win
  _   -> Nothing

parseFromLetter = fromMaybe Invalid . parseLetter
parseFromState = fromMaybe None . parseState

-- string of the form "A B"
parseLetters (x : _ : y : ys) =
  parseLetters ys + case (parseFromLetter x, parseFromState y) of
    (Paper    _, Lose    ) -> parsePair (Paper 2, Rock 1)
    (Paper    _, Draw ) -> parsePair (Paper 2, Paper 2)
    (Paper    _, Win    ) -> parsePair (Paper 2, Scissors 3)
    (Rock     _, Lose    ) -> parsePair (Rock 1, Scissors 3)
    (Rock     _, Draw   ) -> parsePair (Rock 1, Rock 1)
    (Rock     _, Win) -> parsePair (Rock 1, Paper 2)
    (Scissors _, Lose) -> parsePair (Scissors 3, Paper 2)
    (Scissors _, Draw    ) -> parsePair (Scissors 3, Scissors 3)
    (Scissors _, Win   ) -> parsePair (Scissors 3, Rock 1)
    (_, _) -> 0
parseLetters _ = 0


day2Part2 :: IO ()
day2Part2 = do
  args    <- getArgs
  content <- readFile $ head args
  let linesOfLines = lines content
  print $ sum $ map parseLetters linesOfLines
