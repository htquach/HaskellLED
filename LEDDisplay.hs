{-|
Module      : LEDDisplay
Description : Control a LED display with Arduino
Copyright   : (c) Hong Quach, 2015
License     : MIT
            | This source file is licensed under the 
            | "MIT License." Please see the LICENSE
            | in this distribution for license terms.
Maintainer  : hong dot t dot quach at g m a i l dot com
Stability   : experimental
Portability : POSIX

The user would submit a string through the GHCi terminal on the 
computer and it will be scroll across on the LED matrix. 
This project can be extended to drive LED Cube and other large I/O 
projects (e.g. more LED 8xN) with little to no modification. 
The string to be displayed on the matrix can also come from 
another source, such as an email notification or system status.
-}
module LEDDisplay where

import Control.Monad        (forever)
import Control.Monad
import Control.Monad.Trans  (lift, liftIO)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Concurrent   (threadDelay)
import System.IO            (FilePath)
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Numeric              (readHex, showHex, showIntAtBase)
import Data.Char            (intToDigit)
import Data.List            (intercalate)

import CharsLookup

{-|
  Change this value to point to your HW device
  Be sure to install the Arduino driver (www.arduino.cc)
  Here are some commands to help find your arduinoPath
    Windows: Open device manager
    Linux: $ dmesg | grep tty
    Mac: $ ls /dev/tty.*
-}
arduinoPath :: String
arduinoPath = "COM13"

-- | Render the matrix to terminal instead of onto the actual HW
renderMatrixToTerminal :: LEDDisplaySettings  -> [[Bool]] -> IO ()
renderMatrixToTerminal led  m = do
    mapM_ (scrollFrameTerminal m) [0..(((length (m !! 0)) - (width led) - 1) `div` (scrColumnsCount led))]
    threadDelay 1
    where
        scrollFrameTerminal m n= do
            renderFrameOntoTerminal led (matrixToHexFrame (width led) ((scrollLeftFrames led m) !! n))
            threadDelay (scrDelayMSSim led)

-- | Prompt for string and render it
runForever :: LEDDisplaySettings -> SerialPort -> IO (Maybe Int)
runForever led hw = do
    liftIO $ putStrLn "Initialize"
    threadDelay 1000000
    liftIO $ putStrLn "StartUp Logo"
    renderMatrix led hw $ padMatrix led (stringToMatrix "Haskell Logo")
    threadDelay 1000000
    liftIO $ putStr "Ready"
    runMaybeT $ forever $ do
        liftIO $ putStr "Text to display: "
        msg <- liftIO getLine
        when (msg == "exit") $ do
            lift $ renderMatrix (led { scrDelayMSHW = 50000}) hw (stringToMatrix "Bye! »")
            exit
        lift $ renderMatrix led hw $ padMatrix led (stringToMatrix msg)

-- | Render the matrix on the Matrix LED
renderMatrix :: LEDDisplaySettings -> SerialPort -> [[Bool]] -> IO ()
renderMatrix led hw matrix = do
    mapM_ (scrollFrame hw matrix) [0..(((length (matrix !! 0)) - (width led) - 1) `div` (scrColumnsCount led))]
    threadDelay 1
    where
        scrollFrame s m n= do
            renderFrameOntoHW led s (matrixToHexFrame (width led) ((scrollLeftFrames led m) !! n))
            threadDelay (scrDelayMSHW led)

-- | Each matrix has many frames to animate scrolling from right to left
scrollLeftFrames :: LEDDisplaySettings -> [[Bool]] -> [[[Bool]]]
scrollLeftFrames led bss = [[drop ((scrColumnsCount led) * x) bs | bs <- bss] | x <- [0..((length (bss !! 0)) - (width led))]]

-- | The dataToRender is a string of hex encoded bits to send for render.
renderFrameOntoHW :: LEDDisplaySettings -> SerialPort -> String -> IO Int
renderFrameOntoHW led hw dataToRender = do
    liftIO $ print dataToRender
    send hw $ B.pack [fStartChar led]
    mapM_ (send hw) [B.pack [c] | c <- dataToRender]
    send hw $ B.pack [fEndChar led]

-- | Render the hex encoded data onto the terminal as a matrix
renderFrameOntoTerminal :: LEDDisplaySettings -> String -> IO ()
renderFrameOntoTerminal led dataToRender = do
    putStrLn (hexFrameToTerminal (width led) (height led) dataToRender)

-- | Convert a frame in hex format to matrix string
hexFrameToTerminal :: Int -> Int -> String -> String
hexFrameToTerminal w h gs =  
    take 8 (repeat '\n')
    ++ '\n':gs  -- show the hex representation
    ++ '\n':(boarderHorizontal w)
    ++ '\n':(intercalate "\n" 
        [insertSpace (padZeros w (hexToBin  
                (take h (drop (n * h) gs))))
            | n <- [0..(h - 1)]])
    ++ '\n':(boarderHorizontal w)
-- TODO:  Look into fixing the '0' instead of '@' at the begining of each line
-- See TODO in the hextToBin case ('0':xs).  This almost like we need to pad before hexToBin.
boarderHorizontal :: Int -> String
boarderHorizontal w = (take (w * 2) (repeat '='))

insertSpace :: String -> String
insertSpace ""   = ""
insertSpace (x:xs) = x:' ':(insertSpace xs)

hexToBin :: String -> String
hexToBin s = replaceChar ' ' '@' (showIntAtBase 2 intToDigit (fst((readHex s) !! 0)) "") where
    replaceChar :: Char -> Char -> String -> String
    replaceChar m n "" = ""
    replaceChar m n ('1':xs) = m:(replaceChar m n xs)
    replaceChar m n ('0':xs) = n:(replaceChar m n xs)  -- TODO: If 0 is missing, then it won't replace with '@'


padZeros :: Int -> String -> String
padZeros len s | length (s) < len = padZeros len ('0':s)
               | otherwise = s

-- TODO:  Need to deal with the last frame by filling in the Off bits on the right
-- | Truncate a matrix to fit in the frame width and encode it in hex format
matrixToHexFrame :: Int -> [[Bool]] -> String
matrixToHexFrame w matrix = concat [fromIntToHexString (take w row) | row <- matrix]

-- | Convert a String of Bool into a String of Hex  11110000 -> 0xF0
fromIntToHexString :: [Bool] -> String
fromIntToHexString [] = ""
fromIntToHexString bs = showHex (fromBoolToInt(take 4 bs)) (fromIntToHexString (drop 4 bs)) where
    fromBoolToInt :: [Bool] -> Int
    fromBoolToInt bs = fromBoolToIntLSB (reverse bs) where
        fromBoolToIntLSB []         = 0
        fromBoolToIntLSB (True:ds)  = 1 + (2 * fromBoolToIntLSB ds)
        fromBoolToIntLSB (False:ds) = 2 * fromBoolToIntLSB ds


-- | Convert a Hex String to Binary representation "F0" -> "11110000"
displayHexOnTerminal :: String -> String
displayHexOnTerminal _ = "nothing"


-- | Shift the matrix in different directions:  up, down, left, or right ('u', 'd', 'l' or 'r').
shiftMatrix :: Char -> [[Int]] -> [[Int]]
shiftMatrix = undefined

-- | Some test patterns
testPatterns led = [[(c `mod` (r+1)) /= 0 | c <- [0..((width led) - 1)]] | r <- [0..((height led) - 1)]]
testPatternsInvert led = [[(c `mod` (r+1)) == 0 | c <- [0..((width led) - 1)]] | r <- [0..((height led) - 1)]]
matrixAllOn led = [[True | c <- [0..((width led) - 1)]] | r <- [0..((height led) - 1)]]
matrixAllOff led = [[False | c <- [0..((width led) - 1)]] | r <- [0..((height led) - 1)]]


-- | An empty Frame to show all blank
emptyFrame :: LEDDisplaySettings -> [[Bool]]
emptyFrame led = [[lightOff | c <- [1..(width led)]] | r <- [1..(height led)]]

padMatrix :: LEDDisplaySettings -> [[Bool]] -> [[Bool]]
padMatrix led m = concatMatrix (concatMatrix (emptyFrame led) m) (emptyFrame led)

-- | The datatype that would represent an LED Display settings
data LEDDisplaySettings 
    -- | A two dimensions LED display structure
    = LEDMatrix { 
        n :: String
        , width :: Int
        , height :: Int
        , serSpeed :: CommSpeed
        , serPortPath :: FilePath
        , scrDelayMSHW :: Int   -- TODO:  Change to scrDelayMicroSec after removed scrDelayMSSim
        , scrDelayMSSim :: Int  -- TODO:  Remove after added LEDMatrixSim construct
        , scrColumnsCount :: Int
        , fStartChar :: Char
        , fEndChar :: Char
        }
{-  TODO Enable a new construct for simulator
        | LEDMatrixSim {
        n :: String
        , width :: Int
        , height :: Int
        , scrDelayMicroSec :: Int
        , scrColumnsCount :: Int
        , onChar :: Char
        , offChar :: Char}
-}
    deriving (Eq, Show)

-- | The default settings for a Matrix LED
defaultLedMatrix 
    = LEDMatrix {
        n = "32*8 LEDDisplaySettings with medium scroll speed"
        , width = 32, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMSHW =  1000
        , scrDelayMSSim = 50000
        , scrColumnsCount = 1
        , fStartChar = '^'
        , fEndChar = '$'}

fastLedMatrix 
    = LEDMatrix {
        n = "32*8 LEDDisplaySettings with fast scroll speed"
        , width = 32, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMSHW = 10000
        , scrDelayMSSim = 10000
        , scrColumnsCount = 5
        , fStartChar = '^'
        , fEndChar = '$'}

slowLedMatrix 
    = LEDMatrix {
        n = "32*8 LEDDisplaySettings with slow scroll speed"
        , width = 32, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMSHW = 10000
        , scrDelayMSSim = 1000000
        , scrColumnsCount = 5
        , fStartChar = '^'
        , fEndChar = '$'}

wideLedMatrix 
    = LEDMatrix {
        n = "40*8 LEDDisplaySettings"
        , width = 40, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMSHW = 10000
        , scrDelayMSSim = 1000000
        , scrColumnsCount = 5
        , fStartChar = '^'
        , fEndChar = '$'}

-- | An line of code to break out of the 'forever' loop
exit = mzero

-- TODO:  These two classes look the same, so combine them 
-- and distinct the instance by data type.
-- | Simulator of the LED Display by render the output to terminal
class SimLEDDisplay a where
    sPromptAndDisplay :: a ->  IO (Maybe Int)
    
-- | The Hardware 
class LEDDisplay a where
    promptAndDisplay :: a -> IO (Maybe Int)

-- | An instance of the LED Display simulator to render to terminal
instance SimLEDDisplay LEDDisplaySettings where
    sPromptAndDisplay x =   runMaybeT $ forever $ do
        lift $ putStr "Text to display: "
        str <- lift getLine
        when (str == "exit") $ do
            lift $ renderMatrixToTerminal (x { scrDelayMSHW = 50000}) (stringToMatrix "Bye! »")
            exit
        lift $  renderMatrixToTerminal x  $ padMatrix x ((stringToMatrix str))

-- | An instance of the LED Display to render on the hardware
instance LEDDisplay LEDDisplaySettings where
    promptAndDisplay x = do
        withSerial (serPortPath x) defaultSerialSettings { commSpeed = (serSpeed x) } (runForever x)

-- | The main program to run with the Arduino hardware
mainArduino = promptAndDisplay defaultLedMatrix

-- | The main program to run the display on the terminal
main = sPromptAndDisplay defaultLedMatrix