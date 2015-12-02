
import Test.HUnit
import LEDDisplay
import CharsLookup
-- TODO: Move all tests code to another file Test_LEDDisplay.hs
-- | Test code
testPadLeft1 = TestCase (assertEqual "Test padLeft1" (padLeft 'a' 5 "") "aaaaa")
testPadLeft2 = TestCase (assertEqual "Test padLeft2" (padLeft 'a' 5 "bbbbbb") "bbbbbb")
testPadLeft3 = TestCase (assertEqual "Test padLeft3" (padLeft 'a' 5 "bbb") "aabbb")

-- | Initial matrix to be use in tests
frameContent :: [[Bool]]
frameContent =  [(take 4 (repeat False)) | x <- [1..3]]

-- | Helper function to create n columns
padColumns :: Int -> [[Bool]]
padColumns n = [(take n (repeat True)) | x <- [1..3]]

-- | Test cases
padFrameLeft7Expect :: [[Bool]]
padFrameLeft7Expect = concatMatrixWithSeparator (padColumns 2) frameContent

testPadFrameLeft7 = TestCase (assertEqual "Test padFrameLeft7" padFrameLeft7Expect (padFrameLeft 7 frameContent))

padFrameRight7Expect :: [[Bool]]
padFrameRight7Expect = concatMatrixWithSeparator frameContent (padColumns 2)
testPadFrameRight7 = TestCase (assertEqual "Test padFrameRight7" padFrameRight7Expect (padFrameRight 7 frameContent))


padFrameCenter8Expect = concatMatrixWithSeparator (padColumns 1) (concatMatrixWithSeparator frameContent (padColumns 1))
testPadFrameCenter8 = TestCase (assertEqual "Test padFrameCenter8" padFrameCenter8Expect (padFrameCenter 8 frameContent))

padFrameCenter32Expect = concatMatrixWithSeparator (padColumns 13) (concatMatrixWithSeparator frameContent (padColumns 13))
testPadFrameCenter32 = TestCase (assertEqual "Test padFrameCenter32" padFrameCenter32Expect (padFrameCenter 32 frameContent))

runAllTests = do
    runTestTT testPadLeft1
    runTestTT testPadLeft2
    runTestTT testPadLeft3
    runTestTT testPadFrameLeft7
    runTestTT testPadFrameRight7
    runTestTT testPadFrameCenter8
    runTestTT testPadFrameCenter32