{-|
Module      : CharsLookup
Description : Transform from char and string to matrix representation
Copyright   : (c) Hong Quach and Norah Alballa, 2015
License     : MIT
            | This source file is licensed under the
            | "MIT License." Please see the LICENSE
            | in this distribution for license terms.
Maintainer  : hong dot t dot quach at g m a i l dot com
Stability   : experimental
Portability : POSIX

The functions included in this module is mainly for converting
char and string into matrix representation that correspond to the
pixel in the matrix.
-}
module CharsLookup where

import Data.Char    (toLower)
import Data.Maybe   (fromJust, isJust)

{-|
  The bit value that would indicate the LED is on or off.
  The actual value is depended on the polarity of the LED
  and how it was wired.
-}
lightOn :: Bool
lightOn = False
lightOff :: Bool
lightOff = True

-- | Convert a string to the matrix representation.
stringToMatrix :: String -> [[Bool]]
stringToMatrix [] = [[]]
stringToMatrix s@(c:cs) | isJust (specialStrings s) = fromJust (specialStrings s)
                        | otherwise = concatMatrixWithSeparator (letterToMatrix c) (stringToMatrix cs)

-- | Concat two nested lists into one nested list
concatMatrixWithSeparator :: [[Bool]] -> [[Bool]] -> [[Bool]]
concatMatrixWithSeparator  [[]]   [[]]  = [[]]
concatMatrixWithSeparator  [[]]    ts   = ts
concatMatrixWithSeparator   cs    [[]]  = cs
concatMatrixWithSeparator (c:[]) (t:[]) = [ (c ++ lightOff:t) ]
concatMatrixWithSeparator (c:[]) (t:ts) = [ (c ++ lightOff:t) ] ++ ts
concatMatrixWithSeparator (c:cs) (t:[]) =  cs ++ [ (c ++ lightOff:t) ]
concatMatrixWithSeparator (c:cs) (t:ts) = (c ++ lightOff:t) :  concatMatrixWithSeparator cs ts

-- | Trim the extra columns of zeros
trimZeros :: [String] -> [String]
trimZeros ss = trimRightZeros (trimLeftZeros ss ss) ss where
    -- | Remove the left column of all zeros
    trimLeftZeros :: [String] -> [String] -> [String]
    trimLeftZeros lss trimedSS
            | (countNonZero lss) == (countNonZero trimedSS) = trimLeftZeros (dropCol 'L' trimedSS) lss
            | otherwise = trimedSS
    -- | Remove the right column of all zeros
    trimRightZeros :: [String] -> [String] -> [String]
    trimRightZeros rss trimedSS
            | (countNonZero rss) == (countNonZero trimedSS) = trimRightZeros (dropCol 'R' trimedSS) rss
            | otherwise = trimedSS
    -- | Drop the left column
    dropCol :: Char -> [String] -> [String]
    dropCol side xs | side == 'L' = [drop 1 x | x <- xs]
                    | side == 'R' = [take ((length x) - 1) x | x <- xs]
    countNonZero zss = sum [ if z /= '0' then 1 else 0| zs <- zss, z <- zs]

-- | Convert a nested list of Int of 0s and 1s into Bool where 0 to True and 1 to False.
toBoolGrid :: [String] -> [[Bool]]
toBoolGrid charGrid = [[c == '0' |c <- cs] | cs <- charGrid]

-- | Convert a nested list of Bool to Int of 0s and 1s where True to 0 and False to 1.
toIntGrid :: [[Bool]] -> [String]
toIntGrid boolGrid = [[if b then '0' else '1' | b <- bs] | bs <- boolGrid]

-- | Convert a char to the matrix representation in Bool format
letterToMatrix :: Char -> [[Bool]]
letterToMatrix c = toBoolGrid ( charToStrings c)

-- | Convert a char to the matrix representation in the string format.
charToStrings :: Char -> [String]
charToStrings c | c == ' ' = rawCharToStrings c
                | otherwise = trimZeros (rawCharToStrings c)

specialStrings :: String -> Maybe [[Bool]]
specialStrings s
    | map toLower s == "welcome" =
        Just (stringToMatrix "» LED")
    | map toLower s == "what is this?" || map toLower s == "what?"=
        Just (stringToMatrix "This is Haskell LED, and the animation on this 8x32 Matrix LED is driven by program written in Haskell…")
    | map toLower s == "who are we?" || map toLower s == "who?"=
        Just (stringToMatrix "We are Hong Quach and Norah Alballa!…")
    | map toLower s == "what time it is?" || map toLower s == "time?" =
        Just (stringToMatrix "8:30a")
    | map toLower s == "haskell logo" =
        Just (stringToMatrix "»")
    | otherwise = Nothing

-- | The char lookup table
rawCharToStrings :: Char -> [String]
rawCharToStrings c
    -- | Just a space
    | c == ' '  = ["0" | x <- [0..7]]
    -- | Key available on US standard keyboards
    | c == '!'  = ["00000",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00000",
                   "00100",
                   "00000"]

    | c == '"'  = ["01010",
                   "01010",
                   "01010",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000"]

    | c == '#'  = ["00000",
                   "01010",
                   "11111",
                   "01010",
                   "11111",
                   "01010",
                   "00000",
                   "00000"]

    | c == '$'  = ["00100",
                   "01111",
                   "10100",
                   "01110",
                   "00101",
                   "11110",
                   "00100",
                   "00000"]

    | c == '%'  = ["11000",
                   "11001",
                   "00010",
                   "00100",
                   "01000",
                   "10011",
                   "00011",
                   "00000"]

    | c == '&'  = ["01100",
                   "10010",
                   "10100",
                   "01000",
                   "10101",
                   "10010",
                   "01101",
                   "00000"]

    | c == '\'' = ["01100",
                   "00100",
                   "01000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000"]

    | c == '('  = ["00010",
                   "00100",
                   "01000",
                   "01000",
                   "01000",
                   "00100",
                   "00010",
                   "00000"]

    | c == ')'  = ["01000",
                   "00100",
                   "00010",
                   "00010",
                   "00010",
                   "00100",
                   "01000",
                   "00000"]

    | c == '*'  = ["00000",
                   "00100",
                   "10101",
                   "01110",
                   "10101",
                   "00100",
                   "00000",
                   "00000"]

    | c == '+'  = ["00000",
                   "00100",
                   "00100",
                   "11111",
                   "00100",
                   "00100",
                   "00000",
                   "00000"]

    | c == ','  = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "01100",
                   "00100",
                   "01000"]

    | c == '-'  = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "11111",
                   "00000",
                   "00000",
                   "00000"]

    | c == '.'  = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "01100",
                   "01100",
                   "00000"]

    | c == '/'  = ["00000",
                   "00001",
                   "00010",
                   "00100",
                   "01000",
                   "10000",
                   "00000",
                   "00000"]

    | c == '0'  = ["01110",
                   "10001",
                   "10011",
                   "10101",
                   "11001",
                   "10001",
                   "01110",
                   "00000"]

    | c == '1'  = ["00100",
                   "01100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "01110",
                   "00000"]

    | c == '2'  = ["01110",
                   "10001",
                   "00001",
                   "00010",
                   "00100",
                   "01000",
                   "11111",
                   "00000"]

    | c == '3'  = ["11111",
                   "00010",
                   "00100",
                   "00010",
                   "00001",
                   "10001",
                   "01110",
                   "00000"]

    | c == '4'  = ["00010",
                   "00110",
                   "01010",
                   "10010",
                   "11111",
                   "00010",
                   "00010",
                   "00000"]

    | c == '5'  = ["11111",
                   "10000",
                   "11110",
                   "00001",
                   "00001",
                   "10001",
                   "01110",
                   "00000"]

    | c == '6'  = ["00110",
                   "01000",
                   "10000",
                   "11110",
                   "10001",
                   "10001",
                   "01110",
                   "00000"]

    | c == '7'  = ["11111",
                   "00001",
                   "00010",
                   "00100",
                   "01000",
                   "01000",
                   "01000",
                   "00000"]

    | c == '8'  = ["01110",
                   "10001",
                   "10001",
                   "01110",
                   "10001",
                   "10001",
                   "01110",
                   "00000"]

    | c == '9'  = ["01110",
                   "10001",
                   "10001",
                   "01111",
                   "00001",
                   "00010",
                   "01100",
                   "00000"]

    | c == ':'  = ["00000",
                   "00000",
                   "01100",
                   "01100",
                   "00000",
                   "01100",
                   "01100",
                   "00000"]

    | c == ';'  = ["00000",
                   "00000",
                   "01100",
                   "01100",
                   "00000",
                   "01100",
                   "00100",
                   "01000"]

    | c == '<'  = ["00010",
                   "00100",
                   "01000",
                   "10000",
                   "01000",
                   "00100",
                   "00010",
                   "00000"]

    | c == '='  = ["00000",
                   "00000",
                   "11111",
                   "00000",
                   "11111",
                   "00000",
                   "00000",
                   "00000"]

    | c == '>'  = ["10000",
                   "01000",
                   "00100",
                   "00010",
                   "00100",
                   "01000",
                   "10000",
                   "00000"]

    | c == '?'  = ["01110",
                   "10001",
                   "00001",
                   "00010",
                   "00100",
                   "00000",
                   "00100",
                   "00000"]

    | c == '@'  = ["01110",
                   "10001",
                   "00001",
                   "01101",
                   "10101",
                   "10101",
                   "01110",
                   "00000"]

    | c == 'A'  = ["01110",
                   "10001",
                   "10001",
                   "10001",
                   "11111",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'B'  = ["11110",
                   "10001",
                   "10001",
                   "11110",
                   "10001",
                   "10001",
                   "11110",
                   "00000"]

    | c == 'C'  = ["01110",
                   "10001",
                   "10000",
                   "10000",
                   "10000",
                   "10001",
                   "01110",
                   "00000"]

    | c == 'D'  = ["11100",
                   "10010",
                   "10001",
                   "10001",
                   "10001",
                   "10010",
                   "11100",
                   "00000"]

    | c == 'E'  = ["11111",
                   "10000",
                   "10000",
                   "11110",
                   "10000",
                   "10000",
                   "11111",
                   "00000"]

    | c == 'F'  = ["11111",
                   "10000",
                   "10000",
                   "11110",
                   "10000",
                   "10000",
                   "10000",
                   "00000"]

    | c == 'G'  = ["01110",
                   "10001",
                   "10000",
                   "10111",
                   "10001",
                   "10001",
                   "01111",
                   "00000"]

    | c == 'H'  = ["10001",
                   "10001",
                   "10001",
                   "11111",
                   "10001",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'I'  = ["01110",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "01110",
                   "00000"]

    | c == 'J'  = ["00111",
                   "00010",
                   "00010",
                   "00010",
                   "00010",
                   "10010",
                   "01100",
                   "00000"]

    | c == 'K'  = ["10001",
                   "10010",
                   "10100",
                   "11000",
                   "10100",
                   "10010",
                   "10001",
                   "00000"]

    | c == 'L'  = ["10000",
                   "10000",
                   "10000",
                   "10000",
                   "10000",
                   "10000",
                   "11111",
                   "00000"]

    | c == 'M'  = ["10001",
                   "11011",
                   "10101",
                   "10101",
                   "10001",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'N'  = ["10001",
                   "10001",
                   "11001",
                   "10101",
                   "10011",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'O'  = ["01110",
                   "10001",
                   "10001",
                   "10001",
                   "10001",
                   "10001",
                   "01110",
                   "00000"]

    | c == 'P'  = ["11110",
                   "10001",
                   "10001",
                   "11110",
                   "10000",
                   "10000",
                   "10000",
                   "00000"]

    | c == 'Q'  = ["01110",
                   "10001",
                   "10001",
                   "10001",
                   "10101",
                   "10010",
                   "01101",
                   "00000"]

    | c == 'R'  = ["11110",
                   "10001",
                   "10001",
                   "11110",
                   "10100",
                   "10010",
                   "10001",
                   "00000"]

    | c == 'S'  = ["01111",
                   "10000",
                   "10000",
                   "01110",
                   "00001",
                   "00001",
                   "11110",
                   "00000"]

    | c == 'T'  = ["11111",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00000"]

    | c == 'U'  = ["10001",
                   "10001",
                   "10001",
                   "10001",
                   "10001",
                   "10001",
                   "01110",
                   "00000"]

    | c == 'V'  = ["10001",
                   "10001",
                   "10001",
                   "10001",
                   "10001",
                   "01010",
                   "00100",
                   "00000"]

    | c == 'W'  = ["10001",
                   "10001",
                   "10001",
                   "10101",
                   "10101",
                   "10101",
                   "01010",
                   "00000"]

    | c == 'X'  = ["10001",
                   "10001",
                   "01010",
                   "00100",
                   "01010",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'Y'  = ["10001",
                   "10001",
                   "10001",
                   "01010",
                   "00100",
                   "00100",
                   "00100",
                   "00000"]

    | c == 'Z'  = ["11111",
                   "00001",
                   "00010",
                   "00100",
                   "01000",
                   "10000",
                   "11111",
                   "00000"]

    | c == '['  = ["01110",
                   "01000",
                   "01000",
                   "01000",
                   "01000",
                   "01000",
                   "01110",
                   "00000"]

    | c == '\\'  = ["00000",
                   "10000",
                   "01000",
                   "00100",
                   "00010",
                   "00001",
                   "00000",
                   "00000"]

    | c == ']'  = ["01110",
                   "00010",
                   "00010",
                   "00010",
                   "00010",
                   "00010",
                   "01110",
                   "00000"]

    | c == '^'  = ["00100",
                   "01010",
                   "10001",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000"]

    | c == '_'  = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "11111",
                   "00000"]

    | c == '`'  = ["01000",
                   "00100",
                   "00010",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000"]

    | c == 'a'  = ["00000",
                   "00000",
                   "01110",
                   "00001",
                   "01111",
                   "10001",
                   "01111",
                   "00000"]

    | c == 'b'  = ["00000",
                   "10000",
                   "10000",
                   "10110",
                   "11001",
                   "10001",
                   "11110",
                   "00000"]

    | c == 'c'  = ["00000",
                   "00000",
                   "01110",
                   "10000",
                   "10000",
                   "10001",
                   "01110",
                   "00000"]

    | c == 'd'  = ["00000",
                   "00001",
                   "00001",
                   "01101",
                   "10011",
                   "10001",
                   "01111",
                   "00000"]

    | c == 'e'  = ["00000",
                   "00000",
                   "01110",
                   "10001",
                   "11111",
                   "10000",
                   "01110",
                   "00000"]

    | c == 'f'  = ["00000",
                   "00110",
                   "01001",
                   "01000",
                   "11100",
                   "01000",
                   "01000",
                   "00000"]

    | c == 'g'  = ["00000",
                   "00000",
                   "01111",
                   "10001",
                   "10001",
                   "01111",
                   "00001",
                   "01110"]

    | c == 'h'  = ["00000",
                   "10000",
                   "10000",
                   "10110",
                   "11001",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'i'  = ["00000",
                   "00100",
                   "00000",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00000"]

    | c == 'j'  = ["00000",
                   "00010",
                   "00000",
                   "00110",
                   "00010",
                   "00010",
                   "10010",
                   "01100"]

    | c == 'k'  = ["00000",
                   "10000",
                   "10000",
                   "10100",
                   "11000",
                   "10100",
                   "10010",
                   "00000"]

    | c == 'l'  = ["00000",
                   "01100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "01110",
                   "00000"]

    | c == 'm'  = ["00000",
                   "00000",
                   "11010",
                   "10101",
                   "10101",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'n'  = ["00000",
                   "00000",
                   "10110",
                   "11001",
                   "10001",
                   "10001",
                   "10001",
                   "00000"]

    | c == 'o'  = ["00000",
                   "00000",
                   "01110",
                   "10001",
                   "10001",
                   "10001",
                   "01110",
                   "00000"]

    | c == 'p'  = ["00000",
                   "00000",
                   "11110",
                   "10001",
                   "10001",
                   "11110",
                   "10000",
                   "10000"]

    | c == 'q'  = ["00000",
                   "00000",
                   "01111",
                   "10001",
                   "10011",
                   "01101",
                   "00001",
                   "00001"]

    | c == 'r'  = ["00000",
                   "00000",
                   "10110",
                   "11001",
                   "10000",
                   "10000",
                   "10000",
                   "00000"]

    | c == 's'  = ["00000",
                   "00000",
                   "01110",
                   "10000",
                   "01110",
                   "00001",
                   "11110",
                   "00000"]

    | c == 't'  = ["00000",
                   "01000",
                   "11100",
                   "01000",
                   "01000",
                   "01001",
                   "00110",
                   "00000"]

    | c == 'u'  = ["00000",
                   "00000",
                   "10001",
                   "10001",
                   "10001",
                   "10011",
                   "01101",
                   "00000"]

    | c == 'v'  = ["00000",
                   "00000",
                   "10001",
                   "10001",
                   "10001",
                   "01010",
                   "00100",
                   "00000"]

    | c == 'w'  = ["00000",
                   "00000",
                   "10001",
                   "10001",
                   "10101",
                   "10101",
                   "01010",
                   "00000"]

    | c == 'x'  = ["00000",
                   "00000",
                   "10001",
                   "01010",
                   "00100",
                   "01010",
                   "10001",
                   "00000"]

    | c == 'y'  = ["00000",
                   "00000",
                   "10001",
                   "10001",
                   "10001",
                   "01111",
                   "00001",
                   "01110"]

    | c == 'z'  = ["00000",
                   "00000",
                   "11111",
                   "00010",
                   "00100",
                   "01000",
                   "11111",
                   "00000"]

    | c == '{'  = ["00010",
                   "00100",
                   "00100",
                   "01000",
                   "00100",
                   "00100",
                   "00010",
                   "00000"]

    | c == '|'  = ["00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00100",
                   "00000"]

    | c == '}'  = ["01000",
                   "00100",
                   "00100",
                   "00010",
                   "00100",
                   "00100",
                   "01000",
                   "00000"]

    | c == '~'  = ["00000",
                   "00000",
                   "01001",
                   "10101",
                   "10010",
                   "00000",
                   "00000",
                   "00000"]
    -- | The function f ASCII #159
    | c == 'ƒ'  = ["00010",
                   "00100",
                   "00100",
                   "01110",
                   "00100",
                   "00100",
                   "01000",
                   "00000"]
    -- | Horizontal Ellipsis #Alt+0133  U+2026
    | c == '…'  = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "10101",
                   "00000"]

    -- | A subsitute for the Haskell Logo >\=, ASCII #175
    | c == '»'  = ["00000000000",
                   "00000000000",
                   "10010000000",
                   "01001001111",
                   "00100100000",
                   "01001010011",
                   "10010001000",
                   "00000000000"]

    | otherwise = ["00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00000",
                   "00111",
                   "00000"]
