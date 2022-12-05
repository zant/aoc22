import           Test.HUnit
import Day3 (half,  inv, makeMap)
import qualified Data.Map as Map

comp = TestCase $ assertEqual "comp" ([1, 2], [3, 4]) $ half [1, 2,3,4]

invariant = TestCase $ assertEqual "inv" (97, makeMap ['b']) $ inv 'a' $ makeMap ['a', 'b']

tests = TestList [comp, invariant]

main = runTestTT tests
