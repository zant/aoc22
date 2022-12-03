import           Test.HUnit
import Day2 (parseLetter, parseLetters, Move (..))

-- test1 = TestCase (assertEqual "works" 10 (add pairWise))
t1 = TestCase (assertEqual "parses letter" Nothing (parseLetter 'a'))


t2 = TestCase (assertEqual "parses letter" (Just (Rock 1)) (parseLetter 'A'))

t3 = TestCase (assertEqual "parses letter" (Just (Paper 2)) (parseLetter 'B'))

t4 =
  TestCase (assertEqual "parses letter" (Just (Scissors 3)) (parseLetter 'C'))

t5 = TestCase (assertEqual "parses letter" (Just (Rock 1)) (parseLetter 'X'))

t6 = TestCase (assertEqual "parses letter" (Just (Paper 2)) (parseLetter 'Y'))

t7 =
  TestCase (assertEqual "parses letter" (Just (Scissors 3)) (parseLetter 'Z'))

t8 = TestCase
  ( assertEqual "computes properly" 15
  $ parseLetters ['A', 'Y', 'B', 'X', 'C', 'Z']
  )

tests = TestList [t1, t2, t3, t4, t5, t6]

main = runTestTT tests
