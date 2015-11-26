module LEDDisplay  where
------------------------------------------------------------------------
-- Copyright (c) 2015 Hong Quach and Norah Alballa
--
-- This source file is licensed under the "MIT License." Please see the LICENSE
-- in this distribution for license terms.
--
-- Control an 8x8 Matrix of LED. User will enter a string from the prompt
-- to have it scroll on the 8x8 Matrix of LED.
--
-- The user would submit
-- a string through the GHCi terminal on the computer and it will be scroll
-- across on the LED matrix. Depending on the scope of the project, we can
-- also look into adding more animation of the text on the display. This
-- project can be extended to drive LED Cube and other wide I/O projects
-- (e.g. more LED 8xN) with little to no modification. The string to be
-- display on the matrix can also come from another source, such as an
-- email notification or system status.
------------------------------------------------------------------------

import Control.Monad        (forever)
import Control.Monad.Trans  (liftIO)
import Control.Concurrent   (threadDelay)
import System.IO            (FilePath)
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Numeric              (readHex, showHex, showIntAtBase)
import Data.Char            (intToDigit)
import Data.List            (intercalate)

import CharsLookup


-- Constants for Matrix LED frame dimensions, delay between frames, and text scroll speed
frameWidth :: Int
frameWidth = 8 * 4
frameHeight :: Int
frameHeight = 8
frameDelay :: Int
frameDelay = 10
scrollDelayMSHW :: Int
scrollDelayMSHW = 10000
scrollDelayMSSim :: Int
scrollDelayMSSim = 50000
scrollColumnsCount :: Int
scrollColumnsCount = 1

serialPort :: String
serialPort = "COM13"
serialSpeed :: CommSpeed
serialSpeed = CS9600
frameStartChar :: Char
frameStartChar = '^'
frameEndChar :: Char
frameEndChar = '$'



-- The bit value that would indicate the LED is on or off.
-- The actual value is due to the indicate the polarity of the LED
lightOn :: Bool
lightOn = False
lightOff :: Bool
lightOff = True

-- The main program that loop forever on getLine and render it on the Matrix LED
main :: IO Int
main = do
    withSerial serialPort defaultSerialSettings { commSpeed = serialSpeed } runForever

-- Print to screen instead LED Display
mainSimulator :: IO Int
mainSimulator = do
    forever $ do
        liftIO $ putStr "Text to display: "
        msg <- liftIO getLine
        -- TODO: Add exception handling for quiting (Ctrl C or msg == ":q")
        -- See tryJust https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html#v:tryJust
        renderMatrixToTerminal scrollColumnsCount $ padMatrix ((stringToMatrix msg))

renderMatrixToTerminal :: Int -> [[Bool]] -> IO ()
renderMatrixToTerminal c m = do
    mapM_ (scrollFrameTerminal m) [0..(((length (m !! 0)) - frameWidth - 1) `div` c)]
    threadDelay 1
    where
        scrollFrameTerminal m n= do
            renderFrameOntoTerminal (matrixToFrameGrid ((matrixToFrames c m) !! n))
            threadDelay scrollDelayMSSim
    
-- Prompt for string and render it
runForever :: SerialPort -> IO Int
runForever hw = do
    liftIO $ putStrLn "Initialize"
    threadDelay 3000000
    liftIO $ putStrLn "StartUp Logo"
    renderMatrix 1 hw $ padMatrix (toBoolGrid (specialStrings "Haskell Logo"))
    threadDelay 2000000
    liftIO $ putStr "Ready"
    forever $ do
        liftIO $ putStr "Text to display: "
        msg <- liftIO getLine
        -- TODO: Add exception handling for quiting (Ctrl C or msg == ":q")
        -- See tryJust https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html#v:tryJust
        renderMatrix scrollColumnsCount hw $ padMatrix (stringToMatrix msg)

-- Render the matrix on the Matrix LED
renderMatrix :: Int -> SerialPort -> [[Bool]] -> IO ()
renderMatrix scrollSpacing hw matrix = do
    mapM_ (scrollFrame hw matrix) [0..(((length (matrix !! 0)) - frameWidth - 1) `div` scrollSpacing)]
    threadDelay 1
    where
        scrollFrame s m n= do
            renderFrameOntoHW s (matrixToFrameGrid ((matrixToFrames scrollSpacing m) !! n))
            threadDelay scrollDelayMSHW

-- Each matrix has many frames to animate scrolling
matrixToFrames :: Int -> [[Bool]] -> [[[Bool]]]
matrixToFrames scrollSpacing bss = [[drop (scrollSpacing * x) bs | bs <- bss] | x <- [0..((length (bss !! 0)) - frameWidth)]]

-- The dataToRender is a string of hex encoded bits to send for render.
renderFrameOntoHW :: SerialPort -> String -> IO Int
renderFrameOntoHW hw dataToRender = do
    liftIO $ print dataToRender
    send hw $ B.pack [frameStartChar]
    mapM_ (send hw) [B.pack [c] | c <- dataToRender]
    send hw $ B.pack [frameEndChar]

renderFrameOntoTerminal :: String -> IO ()
renderFrameOntoTerminal dataToRender = do
    -- liftIO $ print dataToRender
    putStrLn (hexFrameToTerminal dataToRender)

hexFrameToTerminal :: String -> String
hexFrameToTerminal hs = "\n\n\n" ++ (intercalate "\n" [insertSpace (padZeros frameWidth (hexToBin (take frameHeight ( drop (n * frameHeight) hs)))) | n <- [0..(frameHeight - 1)]])

insertSpace :: String -> String
insertSpace ""   = ""
insertSpace (x:xs) = x:' ':(insertSpace xs)

hexToBin :: String -> String
hexToBin s = replaceChar '.' '@' (showIntAtBase 2 intToDigit (fst((readHex s) !! 0)) "") where
    replaceChar :: Char -> Char -> String -> String
    replaceChar m n "" = ""
    replaceChar m n ('1':xs) = m:(replaceChar m n xs)
    replaceChar m n ('0':xs) = n:(replaceChar m n xs)
    

padZeros :: Int -> String -> String
padZeros len s | length (s) < len = padZeros len ('0':s)
               | otherwise = s

-- Convert a string to the matrix representation.
-- Note add a blank column in between each char.
stringToMatrix :: String -> [[Bool]]
stringToMatrix [] = [[]]
stringToMatrix (c:[]) =  letterToMatrix c
stringToMatrix (c:cs) =  concatMatrix (letterToMatrix c) (stringToMatrix cs)

-- concat two nested lists into one nested list
concatMatrix :: [[Bool]] -> [[Bool]] -> [[Bool]]
concatMatrix [[]]    [[]]  = [[]]
concatMatrix [[]]     ts   = ts
concatMatrix   cs    [[]]  = cs
concatMatrix (c:[]) (t:[]) = [ (c ++ lightOff:t) ]
concatMatrix (c:[]) (t:ts) = [ (c ++ lightOff:t) ] ++ ts
concatMatrix (c:cs) (t:[]) =  cs ++ [ (c ++ lightOff:t) ]
concatMatrix (c:cs) (t:ts) = (c ++ lightOff:t) :  concatMatrix cs ts

-- Truncate a matrix to fit in the frame frameWidth x frameHeight.
matrixToFrameGrid :: [[Bool]] -> String
matrixToFrameGrid matrix = concat [fromIntToHexString (take frameWidth row) | row <- matrix]

-- Convert a String of Bool into a String of Hex  11110000 -> 0xF0
fromIntToHexString :: [Bool] -> String
fromIntToHexString [] = ""
fromIntToHexString bs = showHex (fromBoolToInt(take 4 bs)) (fromIntToHexString (drop 4 bs)) where
    fromBoolToInt :: [Bool] -> Int
    fromBoolToInt bs = fromBoolToIntLSB (reverse bs) where
        fromBoolToIntLSB []     = 0
        fromBoolToIntLSB (True:ds) = 1 + (2 * fromBoolToIntLSB ds)
        fromBoolToIntLSB (False:ds) = 2 * fromBoolToIntLSB ds
        

-- Convert a Hex String to Binary representation "F0" -> "11110000"
displayHexOnTerminal :: String -> String
displayHexOnTerminal _ = "nothing"


-- Shift the matrix in different directions:  up, down, left, or right ('u', 'd', 'l' or 'r').
shiftMatrix :: Char -> [[Int]] -> [[Int]]
shiftMatrix = undefined


-- Some test patterns
testPatterns = [[(c `mod` (r+1)) /= 0 | c <- [0..(frameWidth - 1)]] | r <- [0..(frameHeight - 1)]]
testPatternsInvert = [[(c `mod` (r+1)) == 0 | c <- [0..(frameWidth - 1)]] | r <- [0..(frameHeight - 1)]]
matrixAllOn = [[True | c <- [0..(frameWidth - 1)]] | r <- [0..(frameHeight - 1)]]
matrixAllOff = [[False | c <- [0..(frameWidth - 1)]] | r <- [0..(frameHeight - 1)]]

-- An empty Frame to show all blank
emptyFrame :: [[Bool]]
emptyFrame = [[lightOff | c <- [1..frameWidth]] | r <- [1..frameHeight]]

padMatrix :: [[Bool]] -> [[Bool]]
padMatrix m = concatMatrix (concatMatrix emptyFrame m) emptyFrame

