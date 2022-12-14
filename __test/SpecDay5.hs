import           Test.HUnit
import Day5 (regularParse, movesParser, replace, transition, calculate)

parser = regularParse movesParser

parseMovesBasic = TestCase $ assertEqual "Parse moves" (Right (4,3,1)) (parser "move 4 from 3 to 1")
parseMovesTrivial = TestCase $ assertEqual "group elements" (Right (1,1,1)) (parser "move 1 from 1 to 1")

replaceTrivial = TestCase $ assertEqual "hey" [1,3,3] (replace [1,2,3] 1 3)
replaceBasic = TestCase $ assertEqual "hey" [1,2,3,4,6,6,7] (replace [1,2,3,4,5,6,7] 4 6)

transitionTrivial = TestCase $ assertEqual "hi" ["hell", "hio"] (transition ["hello", "hi"] (1, 1, 2))
transitionBasic = TestCase $ assertEqual "hi" ["he", "hillo"] (transition ["hello", "hi"] (3, 1, 2))
transitionFixture = TestCase $ assertEqual "hi" ["ZND", "MC", "P"] (transition  ["ZN", "MCD", "P"] (1, 2, 1))
transitionFixture2 = TestCase $ assertEqual "hi" ["", "MC", "PDNZ"] (transition  ["ZND", "MC", "P"] (3, 1, 3))
calculateFixture = TestCase $ assertEqual "test" "CMZ" (calculate ["move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"] ["ZN", "MCD", "P"])

main = runTestTT $ TestList [parseMovesBasic, parseMovesTrivial, replaceTrivial, replaceBasic, transitionTrivial, calculateFixture, transitionFixture, transitionFixture2]