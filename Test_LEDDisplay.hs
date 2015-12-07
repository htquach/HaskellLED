import Test.QuickCheck
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

borderHorizontalExpect :: String
borderHorizontalExpect = "================================"
testBorderHorizontal :: Test
testBorderHorizontal = TestCase (assertEqual "Test borderHorizontal" borderHorizontalExpect (borderHorizontal 16))

testInsertSpace :: Test
testInsertSpace = TestCase (assertEqual "Test insertSpace" "i n s e r t S p a c e " (insertSpace "insertSpace"))

hexToBinExpect :: String
hexToBinExpect = ".#..#.#.........#..#..#."
hexToBinInput :: String
hexToBinInput = "b5ff6d"
testHexToBin :: Test
testHexToBin = TestCase (assertEqual "Test hexToBin" hexToBinExpect (hexToBin '#' '.' hexToBinInput))

testPadLeft :: Test
testPadLeft = TestCase (assertEqual "Test padLeft" "aaaaabcdefg" (padLeft 'a' 11 "abcdefg"))

matrixToHexFrameExpect :: String
matrixToHexFrameExpect = "ffffffbfffef1e38c7bdd7efbc18efb5ff6dce30f3ffffff"
matrixToHexFrameInput :: [[Bool]]
matrixToHexFrameInput = stringToMatrix "test again and again"
testMatrixToHexFrame :: Test
testMatrixToHexFrame = TestCase (assertEqual "Test matrixToHexFrame" matrixToHexFrameExpect (matrixToHexFrame 24 matrixToHexFrameInput))

toHexStringInput :: [Bool]
toHexStringInput =
    [False,False,False,True,
     False,False,True, False,
     False,False,True, True,
     False,True, False,False,
     False,True, False,True,
     False,True, True, False,
     False,True, True, True,
     True, False,False,False,
     True, False,False,True,
     False,False,False,False,
     True, False,True, False,
     True, False,True, True,
     True, True, False,False,
     True, True, False,True,
     True, True, True, False,
     True, True, True, True]
testToHexString :: Test
testToHexString = TestCase (assertEqual "Test toHexString" "1234567890abcdef" (toHexString toHexStringInput))

testEmptyFrame :: Test
testEmptyFrame = TestCase (assertEqual "Test emptyFrame" [[True, True] | n <- [1..2]] (emptyFrame 2 2))

padMatrixExpect :: [[Bool]]
padMatrixExpect = [(take 5 (repeat True)) ++ True:[False, True, False] ++ True:(take 5 (repeat True)) | r <- [0..2]]
testPadMatrix :: Test
testPadMatrix = TestCase (assertEqual "Test padMatrix" padMatrixExpect (padMatrix 5 3 [[False, True, False] | r <- [0..2]]))


-- --------- Test CharsLookup.hs ---------

stringToMatrixExpect :: [[Bool]]
stringToMatrixExpect = concatMatrixWithSeparator
    [[True,False,False,False,True],
     [False,True,True,True,False],
     [False,True,True,True,True],
     [False,True,False,False,False],
     [False,True,True,True,False],
     [False,True,True,True,False],
     [True,False,False,False,False],
     [True,True,True,True,True]]
    [[True,False,False,False,True],
     [False,True,True,True,False],
     [False,True,True,True,True],
     [False,True,False,False,False],
     [False,True,True,True,False],
     [False,True,True,True,False],
     [True,False,False,False,False],
     [True,True,True,True,True]]
testStringToMatrix :: Test
testStringToMatrix = TestCase (assertEqual "Test stringToMatrix" stringToMatrixExpect (stringToMatrix "GG"))

concatMatrixWithSeparatorExpect :: [[Bool]]
concatMatrixWithSeparatorExpect = [[False, True, False] | r <- [1..2]]
testConcatMatrixWithSeparator :: Test
testConcatMatrixWithSeparator = TestCase (assertEqual "Test concatMatrixWithSeparator" concatMatrixWithSeparatorExpect (concatMatrixWithSeparator [[False] | r <- [1..2]] [[False] | r <- [1..2]]))


trimZerosExpect :: [String]
trimZerosExpect = ["101" | r <- [1..2]]
trimZerosInput :: [String]
trimZerosInput = ["0000101000000" | r <- [1..2]]
testTrimZeros :: Test
testTrimZeros = TestCase (assertEqual "Test trimZeros" trimZerosExpect (trimZeros trimZerosInput))
testTrimZeros2 = TestCase (assertEqual "Test trimZeros2 " ["1"] (trimZeros ["000001"]))
testTrimZeros3 = TestCase (assertEqual "Test trimZeros3" ["1"] (trimZeros ["100000"]))
testTrimZeros4 = TestCase (assertEqual "Test trimZeros4" [""] (trimZeros ["00000"]))


toBoolGridExpect :: [[Bool]]
toBoolGridExpect = [[True],[False],[True],[False],[False],[False],[False],[True]]
toBoolGridInput :: [String]
toBoolGridInput = ["0", "1", "0", "1", "1", "1", "1", "0"]
testToBoolGrid :: Test
testToBoolGrid = TestCase (assertEqual "Test toBoolGrid" toBoolGridExpect (toBoolGrid toBoolGridInput))
testToBoolGrid2 = TestCase (assertEqual "Test toBoolGrid2" [[False,True,False,True]] (toBoolGrid ["1010"]))

toIntGridExpect :: [String]
toIntGridExpect = ["0", "1", "0", "1", "1", "1", "1", "0"]
toIntGridInput :: [[Bool]]
toIntGridInput = [[True],[False],[True],[False],[False],[False],[False],[True]]
testToIntGrid :: Test
testToIntGrid = TestCase (assertEqual "Test toIntGrid" toIntGridExpect (toIntGrid toIntGridInput))
testToIntGrid2 = TestCase (assertEqual "Test toIntGrid2" ["000","111"] (toIntGrid [[True,True,True],[False,False,False]]))

letterToMatrixExpect :: [[Bool]]
letterToMatrixExpect =
    [[True,False,False,False,True],
     [False,True,True,True,False],
     [False,True,True,True,True],
     [False,True,False,False,False],
     [False,True,True,True,False],
     [False,True,True,True,False],
     [True,False,False,False,False],
     [True,True,True,True,True]]
letterToMatrixExpect2 =
    [[False,False,False],
     [True,False,True],
     [True,False,True],
     [True,False,True],
     [True,False,True],
     [True,False,True],
     [False,False,False],
     [True,True,True]]
testLetterToMatrix :: Test
testLetterToMatrix = TestCase (assertEqual "Test letterToMatrix" letterToMatrixExpect (letterToMatrix 'G'))
testLetterToMatrix2 = TestCase (assertEqual "Test2 letterToMatrix" letterToMatrixExpect2 (letterToMatrix 'I'))


rawCharToStringsExpect :: [String]
rawCharToStringsExpect = ["01110",
                          "10001",
                          "10000",
                          "10111",
                          "10001",
                          "10001",
                          "01111",
                          "00000"]

rawCharToStringsExpect2  = ["01110",
                            "00100",
                            "00100",
                            "00100",
                            "00100",
                            "00100",
                            "01110",
                            "00000"]
testRawCharToStrings :: Test
testRawCharToStrings = TestCase (assertEqual "Test rawCharToStrings" rawCharToStringsExpect (rawCharToStrings 'G'))
testRawCharToStrings2 = TestCase (assertEqual "Test rawCharToStrings" rawCharToStringsExpect2 (rawCharToStrings 'I'))

rawLogoToStringsExpect :: [String]
rawLogoToStringsExpect = ["00000000000",
                          "00000000000",
                          "10010000000",
                          "01001001111",
                          "00100100000",
                          "01001010011",
                          "10010001000",
                          "00000000000"]
testRawLogoToStrings :: Test
testRawLogoToStrings = TestCase (assertEqual "Test rawLogoToStrings" rawLogoToStringsExpect (rawCharToStrings '»'))

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
    ,testBorderHorizontal
    ,testInsertSpace
    ,testHexToBin
    ,testPadLeft
    ,testMatrixToHexFrame
    ,testToHexString
    ,testEmptyFrame
    ,testPadMatrix
    ,testStringToMatrix
    ,testTrimZeros
    ,testTrimZeros2
    ,testTrimZeros3
    ,testTrimZeros4
    ,testConcatMatrixWithSeparator
    ,testToBoolGrid
    ,testToBoolGrid2
    ,testToIntGrid
    ,testToIntGrid2
    ,testLetterToMatrix
    ,testLetterToMatrix2
    ,testRawCharToStrings
    ,testRawCharToStrings2
    ,testRawLogoToStrings
    ]

tests :: Test
tests = test [TestLabel (show x) x | x <- ledDisplayTestCases]

prop_charToStrings :: Char -> Bool
prop_charToStrings x = (charToStrings x == (toIntGrid $ letterToMatrix x))
runQuickCheck = do (quickCheck prop_charToStrings)

rt = do
    runTestTT tests
    quickCheck prop_charToStrings
