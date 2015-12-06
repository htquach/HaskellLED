
{-|
Module      : LEDDisplay
Description : Control a LED display with Arduino
Copyright   : (c) Hong Quach and Norah Alballa, 2015
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
import Control.Monad        (when, mzero)
import Control.Monad.Trans  (lift, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Concurrent   (threadDelay)
import System.IO            (FilePath)
import Numeric              (readHex, showHex, showIntAtBase)
import Data.Char            (intToDigit)
import Data.List            (intercalate)
import System.Locale        (defaultTimeLocale)
import Data.Time.Format     (formatTime)
import System.Time          (getClockTime, toCalendarTime, formatCalendarTime)

-- Serial Port communication with the Arduino board
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B

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
renderMatrixToTerminal led matrix = do
    mapM_ (scrollFrameTerminal m) [0..(((length (m !! 0)) - (width led)) `div` (scrColumnsCount led))]
    threadDelay 1
    where
        scrollFrameTerminal m n = do
            renderFrameOntoTerminal led (matrixToHexFrame (width led) ((scrollLeftFrames (width led) (scrColumnsCount led) m) !! n))
            threadDelay (scrDelayMicroSec led)
        m = padFrameCenter (width led) matrix


-- | Prompt for string and render it to the Arduino Board
promptForever :: LEDDisplaySettings -> SerialPort -> IO (Maybe Int)
promptForever led hw = do
    liftIO $ putStrLn "StartUp Logo"
    renderMatrix led hw $ padMatrix led (stringToMatrix "Haskell Logo")
    liftIO $ putStr "Ready"
    runMaybeT $ forever $ do
        liftIO $ putStr "Text to display: "
        msg <- liftIO getLine
        when (msg == "exit") $ do
            lift $ renderMatrix led hw (stringToMatrix "Bye! »")
            exit
        lift $ renderMatrix led hw $ padMatrix led (stringToMatrix msg)

-- | Display the ticking clock updated every second
clockForever :: LEDDisplaySettings -> SerialPort -> IO (Maybe Int)
clockForever led hw = do
    renderMatrix led hw $ padMatrix led (stringToMatrix "Ctrl+C to stop")
    liftIO $ putStr "Ready"
    forever $ do
        x <- getClockTime
        y <- toCalendarTime x
        liftIO $ renderMatrix led hw $ (concatMatrixWithSeparator (stringToMatrix ("   " ++ (formatCalendarTime defaultTimeLocale "%H:%M:%S" y))) (emptyFrame led))


-- | Prompt for string and render it onto the Terminal
promptForeverTerminal :: LEDDisplaySettings -> IO (Maybe Int)
promptForeverTerminal sim  = runMaybeT $ forever $ do
    liftIO $ putStr "Text to display: "
    str <- liftIO getLine  -- TODO:  implement backspace
    when (str == "exit") $ do
        liftIO $ renderMatrixToTerminal (sim { scrDelayMicroSec = 50000}) (stringToMatrix "Bye »")
        exit
    lift $  renderMatrixToTerminal sim  $ padMatrix sim (stringToMatrix str)

-- | Display the ticking clock updated every second
clockForeverTerminal :: LEDDisplaySettings -> IO (Maybe Int)
clockForeverTerminal sim = forever $ do
    liftIO $ putStr "Showing System Clock.  Use Ctrl+C to exit: "
    x <- getClockTime
    y <- toCalendarTime x
    liftIO $  renderMatrixToTerminal sim (padFrameCenter (width sim) (stringToMatrix (formatCalendarTime defaultTimeLocale "%H:%M:%S" y)))
    threadDelay 1000000


-- | Render the matrix on the Matrix LED
renderMatrix :: LEDDisplaySettings -> SerialPort -> [[Bool]] -> IO ()
renderMatrix led hw matrix = do
    mapM_ (scrollFrame hw matrix) [0..(((length (matrix !! 0)) - (width led) - 1) `div` (scrColumnsCount led))]
    threadDelay 1
    where
        scrollFrame s m n= do
            renderFrameOntoHW led s (matrixToHexFrame (width led) ((scrollLeftFrames (width led) (scrColumnsCount led) m) !! n))
            threadDelay (scrDelayMicroSec led)


-- | Each matrix has many frames to animate scrolling from right to left
scrollLeftFrames :: Int -> Int -> [[Bool]] -> [[[Bool]]]
scrollLeftFrames w step bss = [[take w (drop (step * x) bs) | bs <- bss] | x <- [0..((length (bss !! 0)) - w)]]


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
    putStrLn (hexFrameToTerminal (onChar led) (offChar led) (width led) (height led) dataToRender)


-- | Convert a frame in hex format to matrix string
hexFrameToTerminal :: Char -> Char -> Int -> Int -> String -> String
hexFrameToTerminal onC offC w h gs =
    take 8 (repeat '\n') -- Some blank lines to separate each frame in the output
    ++ '\n':gs  -- show the hex representation
    ++ '\n':(borderHorizontal w)
    ++ '\n':(intercalate "\n"
        [insertSpace (padLeft onC w (hexToBin onC offC
                (take (w `div` 4) (drop (n * (w `div` 4)) gs))))
            | n <- [0..(h - 1)]])
    ++ '\n':(borderHorizontal w)


-- | A horizontal border to wrap arround the frame
borderHorizontal :: Int -> String
borderHorizontal w = (take (w * 2) (repeat '='))


-- | A blank space between each char including the last one.
insertSpace :: String -> String
insertSpace ""   = ""
insertSpace (x:xs) = x:' ':(insertSpace xs)


-- | Convert a hex encoded string into a string of binary with OnChar and offChar
hexToBin :: Char -> Char -> String -> String
hexToBin onC offC s = replaceChar onC offC (showIntAtBase 2 intToDigit (fst((readHex s) !! 0)) "") where
    replaceChar :: Char -> Char -> String -> String
    replaceChar m n "" = ""
    replaceChar m n ('1':xs) = n:(replaceChar m n xs)
    replaceChar m n ('0':xs) = m:(replaceChar m n xs)


{-| Pad the left side of the string with the specified Char until it satistfies
  the specified length
-}
padLeft :: Char -> Int -> String -> String
padLeft onC len s | length (s) < len = padLeft onC len (onC:s)
                  | otherwise = s


-- | Truncate a matrix to fit in the frame then convert it to a hex string
matrixToHexFrame :: Int -> [[Bool]] -> String
matrixToHexFrame w matrix = concat [toHexString (take w row) | row <- matrix]


-- | Convert a String of Bools into a String of Hex  11110000 -> 0xF0
toHexString :: [Bool] -> String
toHexString [] = ""
toHexString bs = showHex (fromBoolToInt(take 4 bs)) (toHexString (drop 4 bs)) where
    fromBoolToInt :: [Bool] -> Int
    fromBoolToInt bs = fromBoolToIntLSB (reverse bs) where
        fromBoolToIntLSB [] = 0
        fromBoolToIntLSB (x:ds) | x == lightOff = 1 + (2 * fromBoolToIntLSB ds)
                                | otherwise     = 2 * fromBoolToIntLSB ds


-- | An empty Frame to show all blank
emptyFrame :: LEDDisplaySettings -> [[Bool]]
emptyFrame led = [[lightOff | c <- [1..(width led)]] | r <- [1..(height led)]]


-- | Pad the matrix with empty frame at the beginning and at the end of the matrix
padMatrix :: LEDDisplaySettings -> [[Bool]] -> [[Bool]]
padMatrix led m = concatMatrixWithSeparator (concatMatrixWithSeparator (emptyFrame led) m) (emptyFrame led)


-- | Pad the left side of the frame with blank columns to fill the frame
padFrameLeft :: Int -> [[Bool]] -> [[Bool]]
padFrameLeft w bss = [[lightOff | x <- [(length bs)..(w-1)]] ++ bs | bs <- bss]


-- | Pad the right side of the frame with blank columns to fill the frame
padFrameRight :: Int -> [[Bool]] -> [[Bool]]
padFrameRight w bss = [bs ++ [lightOff | x <- [(length bs)..(w-1)]] | bs <- bss]


-- | Pad both sides of the frame with blank columns to fill the frame
padFrameCenter :: Int -> [[Bool]] -> [[Bool]]
padFrameCenter w bss = padFrameLeft w (padFrameRight ((length (bss!!0))+((w - (length (bss!!0))) `div` 2)) bss)


{-| The datatype that would represent an LED Display settings
  the constructor that would represent an LED Display for the hardware
-}
data LEDDisplaySettings
    -- | A constructor for a two dimensions LED display (matrix)
    = LEDMatrix {
        name :: String
        , width :: Int
        , height :: Int
        , scrDelayMicroSec :: Int
        , scrColumnsCount :: Int
        , serSpeed :: CommSpeed
        , serPortPath :: FilePath
        , fStartChar :: Char
        , fEndChar :: Char
        }
    -- | A constructor for simulating a two dimensions display
    | LEDMatrixSim {
        name :: String
        , width :: Int
        , height :: Int
        , scrDelayMicroSec :: Int
        , scrColumnsCount :: Int
        , onChar :: Char
        , offChar :: Char}

    deriving (Eq, Show)

-- | The default settings for an 8x32 Matrix LED
defaultLedMatrix
    = LEDMatrix {
        name = "32*8 LEDDisplaySettings with medium scroll speed"
        , width = 32, height = 8
        , scrDelayMicroSec =  1000
        , scrColumnsCount = 1
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , fStartChar = '^'
        , fEndChar = '$'}

-- | The default setting for a Terminal display 8x32
defaultSimulator
    = LEDMatrixSim {
        name = "32*8 Simulator with medium scroll speed"
        , width = 32, height = 8
        , scrDelayMicroSec = 50000
        , scrColumnsCount = 1
        , onChar = '#'
        , offChar = ' '}

-- | This is for a fast scrolling 8x32 matrix LED
fastLedMatrix
    = LEDMatrix {
        name = "32*8 LEDDisplaySettings with fast scroll speed"
        , width = 32, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMicroSec = 10000
        , scrColumnsCount = 5
        , fStartChar = '^'
        , fEndChar = '$'}

-- | This is for a fast scrolling 8x32 terminal simulator
fastSimulator
    = LEDMatrixSim {
        name = "32*8 Simulator with fast scroll speed"
        , width = 32, height = 8
        , scrDelayMicroSec = 10000
        , scrColumnsCount = 5
        , onChar = '@'
        , offChar = ' '}

-- | This is for a slow scrolling 8x32 matrix LED
slowLedMatrix
    = LEDMatrix {
        name = "32*8 LEDDisplaySettings with slow scroll speed"
        , width = 32, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMicroSec = 100000
        , scrColumnsCount = 1
        , fStartChar = '^'
        , fEndChar = '$'}

-- | This is for a slow scrolling 8x32 terminal simulator
slowSimulator
    = LEDMatrixSim {
        name = "32*8 Simulator with slow scroll speed"
        , width = 32, height = 8
        , scrDelayMicroSec = 100000
        , scrColumnsCount = 1
        , onChar = '@'
        , offChar = ' '}

-- | This is for a wide 8x40 matrix LED
wideLedMatrix
    = LEDMatrix {
        name = "40*8 LEDDisplaySettings"
        , width = 40, height = 8
        , serSpeed = CS9600
        , serPortPath = arduinoPath
        , scrDelayMicroSec = 10000
        , scrColumnsCount = 5
        , fStartChar = '^'
        , fEndChar = '$'}

-- | This is for a wide 8x40 terminal simulator
wideSimulator
    = LEDMatrixSim {
        name = "32*8 Simulator with medium scroll speed"
        , width = 40, height = 8
        , scrDelayMicroSec = 50000
        , scrColumnsCount = 1
        , onChar = '@'
        , offChar = ' '}


-- | An line of code to break out of the 'forever' loop
exit = mzero


-- | An LED Display class that has various methods for displaying
class LEDDisplay a where
    -- | Prompt user to input a string to be shown on the display
    promptAndDisplay :: a ->  IO (Maybe Int)
    -- | Display a ticking clock
    clock :: a -> IO (Maybe Int)
           
           
-- | An instance of the LED Display simulator to render to terminal
instance LEDDisplay LEDDisplaySettings where
    promptAndDisplay sim@(LEDMatrixSim {})
        = promptForeverTerminal sim
    promptAndDisplay hwmatrix@(LEDMatrix {})
        = do withSerial (serPortPath hwmatrix)
                        defaultSerialSettings { commSpeed = (serSpeed hwmatrix) }
                        (promptForever hwmatrix)
    clock sim@(LEDMatrixSim {})
        = clockForeverTerminal sim
    clock hwmatrix@(LEDMatrix {})
        = do withSerial (serPortPath hwmatrix)
                        defaultSerialSettings { commSpeed = (serSpeed hwmatrix) }
                        (clockForever hwmatrix)
   
   
-- | The main program to run with the Arduino hardware
runArduino = promptAndDisplay defaultLedMatrix


-- | The main program to run the display on the terminal
runTerminal = promptAndDisplay defaultSimulator


-- | Display the current time on the terminal
runTerminalClock = clock wideSimulator