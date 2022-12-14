import           Test.HUnit
import Day6 (day6)

trivial = TestCase $ assertEqual "Parse moves" (1) (id 1)

main = runTestTT $ TestList [trivial]