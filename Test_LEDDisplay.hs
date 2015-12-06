
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

padFrameLeft7Expect :: [[Bool]]
padFrameLeft7Expect = concatMatrixWithSeparator (padColumns 2) frameContent

testPadFrameLeft7 :: Test
testPadFrameLeft7 = TestCase (assertEqual "Test padFrameLeft7" padFrameLeft7Expect (padFrameLeft 7 frameContent))

padFrameRight7Expect :: [[Bool]]
padFrameRight7Expect = concatMatrixWithSeparator frameContent (padColumns 2)
testPadFrameRight7 = TestCase (assertEqual "Test padFrameRight7" padFrameRight7Expect (padFrameRight 7 frameContent))

padFrameCenter8Expect :: [[Bool]]
padFrameCenter8Expect = concatMatrixWithSeparator (padColumns 1) (concatMatrixWithSeparator frameContent (padColumns 1))
testPadFrameCenter8 :: Test
testPadFrameCenter8 = TestCase (assertEqual "Test padFrameCenter8" padFrameCenter8Expect (padFrameCenter 8 frameContent))

padFrameCenter32Expect :: [[Bool]]
padFrameCenter32Expect = concatMatrixWithSeparator (padColumns 13) (concatMatrixWithSeparator frameContent (padColumns 13))
testPadFrameCenter32 :: Test
testPadFrameCenter32 = TestCase (assertEqual "Test padFrameCenter32" padFrameCenter32Expect (padFrameCenter 32 frameContent))

scrollLeftFramesExpect :: [[[Bool]]]
scrollLeftFramesExpect = [[[True,  True,  False] | r <- [0..2]]
                         ,[[True,  False, False] | r <- [0..2]]
                         ,[[False, False, True ] | r <- [0..2]]
                         ,[[False, True,  True ] | r <- [0..2]]]
scrollLeftFramesInput :: [[Bool]]
scrollLeftFramesInput = [[True, True, False, False, True, True] | r <- [0..2]]
testScrollLeftFrames :: Test
testScrollLeftFrames = TestCase (assertEqual "Test testScrollLeftFrames" scrollLeftFramesExpect (scrollLeftFrames 3 1 scrollLeftFramesInput))

hexFrameToTerminalExpect :: String
hexFrameToTerminalExpect = take 9 (repeat '\n')
    ++ "ffffffbfffef1e38c7bdd7efbc18efb5ff6dce30f3ffffff\n"
    ++ "================================================\n"
    ++ ". . . . . . . . . . . . . . . . . . . . . . . . \n"
    ++ ". # . . . . . . . . . . . . . . . . . # . . . . \n"
    ++ "# # # . . . . # # # . . . # # # . . # # # . . . \n"
    ++ ". # . . . . # . . . # . # . . . . . . # . . . . \n"
    ++ ". # . . . . # # # # # . . # # # . . . # . . . . \n"
    ++ ". # . . # . # . . . . . . . . . # . . # . . # . \n"
    ++ ". . # # . . . # # # . . # # # # . . . . # # . . \n"
    ++ ". . . . . . . . . . . . . . . . . . . . . . . . \n"
    ++ "================================================"

hexFrameToTerminalInput :: String
hexFrameToTerminalInput = "ffffffbfffef1e38c7bdd7efbc18efb5ff6dce30f3ffffff"
testHexFrameToTerminal :: Test
testHexFrameToTerminal = TestCase (assertEqual "Test hexFrameToTerminal" hexFrameToTerminalExpect (hexFrameToTerminal '#' '.' 24 8 hexFrameToTerminalInput))

ledDisplayTestCases :: [Test]
ledDisplayTestCases = [
    testPadLeft1
    ,testPadLeft2
    ,testPadLeft3
    ,testPadFrameLeft7
    ,testPadFrameRight7
    ,testPadFrameCenter8
    ,testPadFrameCenter32
    ,testScrollLeftFrames
    ,testHexFrameToTerminal
    ]

tests :: Test
tests = test [TestLabel (show x) x | x <- ledDisplayTestCases]

rt = do
    runTestTT tests
