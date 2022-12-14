import           Test.HUnit
import Day6 (day6, hasDuplicate, findDuplicates)

fixture = ["bvwbjplbgvbhsrlpgdmjqwftvncz",
          "nppdvjthqldpwncqszvftbrmjlhg",
          "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
          "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

hasDuplicateTrue = TestCase $ assertEqual "duplicate True" (True) (hasDuplicate [1,2,2,3])
hasDuplicateFalse = TestCase $ assertEqual "duplicate False" (False) (hasDuplicate [1,2,3])
findDuplicates0 = TestCase $ assertEqual "fixture" 7 (findDuplicates 0 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
findDuplicatesFixture = TestCase $ assertEqual "fixture map" [5,6,10,11] (map (findDuplicates 0 4) fixture)

main = runTestTT $ TestList [hasDuplicateTrue, hasDuplicateFalse, findDuplicates0, findDuplicatesFixture]