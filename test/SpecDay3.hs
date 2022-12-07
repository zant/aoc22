import           Test.HUnit
import Day3Part2 (group, hasCommon, commonMany, execCommonMany, ascii, stringsToAscii)
import Test.HUnit (Test(TestList))
import Utils (makeMap)

-- group
groupTestTrivial = TestCase $ assertEqual "group elements" [[1]] $ group 1 [1]
groupTestPairs = TestCase $ assertEqual "group elements" [[1,2],[1,2]] $ group 2 [1,2,1,2]
groupTestOddPairs = TestCase $ assertEqual "group elements" [[1,2],[1,2],[3]] $ group 2 [1,2,1,2,3]

-- hasCommon
hasCommonBasic = TestCase $ assertEqual "common identifies common elements" ['a'] $ hasCommon (makeMap ['a','b','c']) ['a','z','d']

-- commonMany
commonManyTrivial = TestCase $ assertEqual "commonMany finds common in mutiple arrays" ['a'] (commonMany "abc" ["aaaaa"])
commonManyBasic = TestCase $ assertEqual "commonMany finds common in mutiple arrays" ['a'] (commonMany "abc" ["ayyyya", "azzwee", "hyyazz"])
commonManyTwo = TestCase $ assertEqual "commonMany finds common in mutiple arrays" "ba" (commonMany "abc" ["ayyybya", "azzbwee", "hybyazz"])
commonManyExhaustive = TestCase $ assertEqual "commonMany finds common in mutiple arrays" "ba" (commonMany "abc" ["ayyybya", "azzbwee", "hybyazz"])

-- execCommonMany
execCommonManyTwo = TestCase $ assertEqual "commonMany finds common in mutiple arrays" "ba" $ execCommonMany ["abc", "ayyybya", "azzbwee", "hybyazz"]

-- ascii
asciiBasic = TestCase $ assertEqual "returns correct int" 1  $ ascii 'a'
asciiBasicUpper = TestCase $ assertEqual "returns correct int" 27  $ ascii 'A'
asciiStringsBasic = TestCase $ assertEqual "returns correct ints" [1,27]  $ stringsToAscii ["a", "A"]

main = runTestTT $ TestList [groupTestTrivial, groupTestPairs, groupTestOddPairs, hasCommonBasic, commonManyTrivial, commonManyBasic, commonManyTwo, commonManyExhaustive, execCommonManyTwo, asciiBasic, asciiBasicUpper, asciiStringsBasic]