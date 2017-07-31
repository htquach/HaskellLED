"""
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
"""


#  The bit value that would indicate the LED is on or off.
#  The actual value is depended on the polarity of the LED
#  and how it was wired.
lightOn = False
lightOff = True


def stringToMatrix(s):
    """ Convert a string to the matrix representation.
    stringToMatrix :: String -> [[Bool]]
    """
    if s:
        result = specialStrings(s)
        if result:
            return result
        else:
            return concatMatrixWithSeparator(s[0], stringToMatrix(s[1:]))
    else:
        return [[]]

def concatMatrixWithSeparator(cs, ts):
    """Concat two nested lists into one nested list
    concatMatrixWithSeparator :: [[Bool]] -> [[Bool]] -> [[Bool]]
    concatMatrixWithSeparator  [[]]   [[]]  = [[]]
    concatMatrixWithSeparator  [[]]    ts   = ts
    concatMatrixWithSeparator   cs    [[]]  = cs
    concatMatrixWithSeparator (c:[]) (t:[]) = [ (c ++ lightOff:t) ]
    concatMatrixWithSeparator (c:[]) (t:ts) = [ (c ++ lightOff:t) ] ++ ts
    concatMatrixWithSeparator (c:cs) (t:[]) =  cs ++ [ (c ++ lightOff:t) ]
    concatMatrixWithSeparator (c:cs) (t:ts) = (c ++ lightOff:t) :  concatMatrixWithSeparator cs ts
    """
    if cs == [[]]:
        return ts
    if ts == [[]]:
        return cs
    if not cs[1:] and not ts[1:]:
        return [cs[0] + [lightOff] + ts[0]]
    if not cs[1:] and ts[1:]:
        return [cs[0] + [lightOff] + ts[0]] + ts[1:]
    if cs[1:] and not ts[1:]:
        return cs[1:] + [cs[0] + [lightOff] + ts[0]]
    if cs[1:] and ts[1:]:
        return [(cs[0] + [lightOff, ts[0]])] + concatMatrixWithSeparator(cs[1:], ts[1:])


def trimZeros(ss):
    """Trim the extra columns of zeros
    trimZeros :: [String] -> [String]
    trimZeros ss
    """
    # All zeros, just return empty
    if countNonZero(ss) == 0:
        return ["" for x in ss]
    else:
        return trimRightZeros(trimLeftZeros(ss, ss), ss)
        # where
        # # Remove the left column of all zeros
        # trimLeftZeros :: [String] -> [String] -> [String]
        # trimLeftZeros lss trimedSS
        #         | (countNonZero lss) == (countNonZero trimedSS) = trimLeftZeros (dropCol 'L' trimedSS) lss
        #         | otherwise = trimedSS
        # # Remove the right column of all zeros
        # trimRightZeros :: [String] -> [String] -> [String]
        # trimRightZeros rss trimedSS
        #         | (countNonZero rss) == (countNonZero trimedSS) = trimRightZeros (dropCol 'R' trimedSS) rss
        #         | otherwise = trimedSS
        # # Drop the left column
        # dropCol :: Char -> [String] -> [String]
        # dropCol side xs | side == 'L' = [drop 1 x | x <- xs]
        #                 | side == 'R' = [take ((length x) - 1) x | x <- xs]
        # countNonZero zss = sum [ if z /= '0' then 1 else 0| zs <- zss, z <- zs]


def trimLeftZeros(lss, trimedSS):
    """ trimLeftZeros :: [String] -> [String] -> [String]
        trimLeftZeros lss trimedSS
                | (countNonZero lss) == (countNonZero trimedSS) = trimLeftZeros (dropCol 'L' trimedSS) lss
                | otherwise = trimedSS
    """
    if countNonZero(lss) == countNonZero(trimedSS):
        return trimLeftZeros(dropCol('L', trimedSS), lss)
    else:
        return trimedSS

def trimRightZeros(rss, trimedSS):
    if countNonZero(rss) == countNonZero(trimedSS):
        return trimLeftZeros(dropCol('R', trimedSS), rss)
    else:
        return trimedSS

def dropCol(side, xs):
    """ Drop the left column
        dropCol :: Char -> [String] -> [String]
        dropCol side xs | side == 'L' = [drop 1 x | x <- xs]
                        | side == 'R' = [take ((length x) - 1) x | x <- xs]
    """
    if side == 'L':
        return [x[1:] for x in xs]
    elif side == 'R':
        return [x[:-1] for x in xs]

def countNonZero(zss):
    return sum([1 for zs in zss for z in zs if z != '0'])

def toBoolGrid(charGrid):
    """ Convert a nested list of Int of 0s and 1s into Bool where 0 to True and 1 to False.
    toBoolGrid :: [String] -> [[Bool]]
    toBoolGrid charGrid = [[c == '0' |c <- cs] | cs <- charGrid]
    """
    return [[c == '0' for c in cs] for cs in charGrid]

def toIntGrid(boolGrid):
    """ Convert a nested list of Bool to Int of 0s and 1s where True to 0 and False to 1.
    toIntGrid :: [[Bool]] -> [String]
    toIntGrid boolGrid = [[if b then '0' else '1' | b <- bs] | bs <- boolGrid]
    """
    return [['0' if b else '1' for b in bs] for bs in boolGrid]

def letterToMatrix(c):
    """ Convert a char to the matrix representation in Bool format
    letterToMatrix :: Char -> [[Bool]]
    letterToMatrix c = toBoolGrid ( charToStrings c)
    """
    return toBoolGrid(charToStrings(c))

def charToStrings(c):
    """Convert a char to the matrix representation in the string format.
    charToStrings :: Char -> [String]
    """
    if c == ' ':
        return rawCharToStrings(c)
    else:
        return trimZeros(rawCharToStrings(c))

def specialStrings(s):
    """A set of special strings that would map to different text
    specialStrings :: String -> Maybe [[Bool]]
    specialStrings s
    """
    s = s.lower()
    if s == "welcome":
        return stringToMatrix("» LED")
    elif s == "what is this?" or s == "what?":
        return stringToMatrix("This is Haskell LED.  The animation on this 8x32 Matrix LED is driven by a program written in Haskell…")
    elif s == "who are we?" or s == "who?":
        return stringToMatrix("We are Hong Quach and Norah Alballa!…")
    elif s == "haskell logo":
        return stringToMatrix("»")
    else:
        return None


def rawCharToStrings(c):
    """The char lookup table"""
    # Just a space
    if c == ' ':  
        return ["0" for x in range(7)]
    # Key available on US standard keyboards
    elif c == '!': 
        return["00000",
               "00100",
               "00100",
               "00100",
               "00100",
               "00000",
               "00100",
               "00000"]

    elif c == '"':
        return["01010",
               "01010",
               "01010",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000"]

    elif c == '#':
        return["00000",
               "01010",
               "11111",
               "01010",
               "11111",
               "01010",
               "00000",
               "00000"]

    elif c == '$':
        return["00100",
               "01111",
               "10100",
               "01110",
               "00101",
               "11110",
               "00100",
               "00000"]

    elif c == '%':
        return["11000",
               "11001",
               "00010",
               "00100",
               "01000",
               "10011",
               "00011",
               "00000"]

    elif c == '&':
        return["01100",
               "10010",
               "10100",
               "01000",
               "10101",
               "10010",
               "01101",
               "00000"]

    elif c == '\'':
        return["01100",
               "00100",
               "01000",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000"]

    elif c == '(':
        return["00010",
               "00100",
               "01000",
               "01000",
               "01000",
               "00100",
               "00010",
               "00000"]

    elif c == ')':
        return["01000",
               "00100",
               "00010",
               "00010",
               "00010",
               "00100",
               "01000",
               "00000"]

    elif c == '*':
        return["00000",
               "00100",
               "10101",
               "01110",
               "10101",
               "00100",
               "00000",
               "00000"]

    elif c == '+':
        return["00000",
               "00100",
               "00100",
               "11111",
               "00100",
               "00100",
               "00000",
               "00000"]

    elif c == ',':
        return["00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "01100",
               "00100",
               "01000"]

    elif c == '-':
        return["00000",
               "00000",
               "00000",
               "00000",
               "11111",
               "00000",
               "00000",
               "00000"]

    elif c == '.':
        return["00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "01100",
               "01100",
               "00000"]

    elif c == '/':
        return["00000",
               "00001",
               "00010",
               "00100",
               "01000",
               "10000",
               "00000",
               "00000"]

    elif c == '0':
        return["01110",
               "10001",
               "10011",
               "10101",
               "11001",
               "10001",
               "01110",
               "00000"]

    elif c == '1':
        return["00100",
               "01100",
               "00100",
               "00100",
               "00100",
               "00100",
               "01110",
               "00000"]

    elif c == '2':
        return["01110",
               "10001",
               "00001",
               "00010",
               "00100",
               "01000",
               "11111",
               "00000"]

    elif c == '3':
        return["11111",
               "00010",
               "00100",
               "00010",
               "00001",
               "10001",
               "01110",
               "00000"]

    elif c == '4':
        return["00010",
               "00110",
               "01010",
               "10010",
               "11111",
               "00010",
               "00010",
               "00000"]

    elif c == '5':
        return["11111",
               "10000",
               "11110",
               "00001",
               "00001",
               "10001",
               "01110",
               "00000"]

    elif c == '6':
        return["00110",
               "01000",
               "10000",
               "11110",
               "10001",
               "10001",
               "01110",
               "00000"]

    elif c == '7':
        return["11111",
               "00001",
               "00010",
               "00100",
               "01000",
               "01000",
               "01000",
               "00000"]

    elif c == '8':
        return["01110",
               "10001",
               "10001",
               "01110",
               "10001",
               "10001",
               "01110",
               "00000"]

    elif c == '9':
        return["01110",
               "10001",
               "10001",
               "01111",
               "00001",
               "00010",
               "01100",
               "00000"]

    elif c == ':':
        return["00000",
               "00000",
               "01100",
               "01100",
               "00000",
               "01100",
               "01100",
               "00000"]

    elif c == ';':
        return["00000",
               "00000",
               "01100",
               "01100",
               "00000",
               "01100",
               "00100",
               "01000"]

    elif c == '<':
        return["00010",
               "00100",
               "01000",
               "10000",
               "01000",
               "00100",
               "00010",
               "00000"]

    elif c == '=':
        return["00000",
               "00000",
               "11111",
               "00000",
               "11111",
               "00000",
               "00000",
               "00000"]

    elif c == '>':
        return["10000",
               "01000",
               "00100",
               "00010",
               "00100",
               "01000",
               "10000",
               "00000"]

    elif c == '?':
        return["01110",
               "10001",
               "00001",
               "00010",
               "00100",
               "00000",
               "00100",
               "00000"]

    elif c == '@':
        return["01110",
               "10001",
               "00001",
               "01101",
               "10101",
               "10101",
               "01110",
               "00000"]

    elif c == 'A':
        return["01110",
               "10001",
               "10001",
               "10001",
               "11111",
               "10001",
               "10001",
               "00000"]

    elif c == 'B':
        return["11110",
               "10001",
               "10001",
               "11110",
               "10001",
               "10001",
               "11110",
               "00000"]

    elif c == 'C':
        return["01110",
               "10001",
               "10000",
               "10000",
               "10000",
               "10001",
               "01110",
               "00000"]

    elif c == 'D':
        return["11100",
               "10010",
               "10001",
               "10001",
               "10001",
               "10010",
               "11100",
               "00000"]

    elif c == 'E':
        return["11111",
               "10000",
               "10000",
               "11110",
               "10000",
               "10000",
               "11111",
               "00000"]

    elif c == 'F':
        return["11111",
               "10000",
               "10000",
               "11110",
               "10000",
               "10000",
               "10000",
               "00000"]

    elif c == 'G':
        return["01110",
               "10001",
               "10000",
               "10111",
               "10001",
               "10001",
               "01111",
               "00000"]

    elif c == 'H':
        return["10001",
               "10001",
               "10001",
               "11111",
               "10001",
               "10001",
               "10001",
               "00000"]

    elif c == 'I':
        return["01110",
               "00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "01110",
               "00000"]

    elif c == 'J':
        return["00111",
               "00010",
               "00010",
               "00010",
               "00010",
               "10010",
               "01100",
               "00000"]

    elif c == 'K':
        return["10001",
               "10010",
               "10100",
               "11000",
               "10100",
               "10010",
               "10001",
               "00000"]

    elif c == 'L':
        return["10000",
               "10000",
               "10000",
               "10000",
               "10000",
               "10000",
               "11111",
               "00000"]

    elif c == 'M':
        return["10001",
               "11011",
               "10101",
               "10101",
               "10001",
               "10001",
               "10001",
               "00000"]

    elif c == 'N':
        return["10001",
               "10001",
               "11001",
               "10101",
               "10011",
               "10001",
               "10001",
               "00000"]

    elif c == 'O':
        return["01110",
               "10001",
               "10001",
               "10001",
               "10001",
               "10001",
               "01110",
               "00000"]

    elif c == 'P':
        return["11110",
               "10001",
               "10001",
               "11110",
               "10000",
               "10000",
               "10000",
               "00000"]

    elif c == 'Q':
        return["01110",
               "10001",
               "10001",
               "10001",
               "10101",
               "10010",
               "01101",
               "00000"]

    elif c == 'R':
        return["11110",
               "10001",
               "10001",
               "11110",
               "10100",
               "10010",
               "10001",
               "00000"]

    elif c == 'S':
        return["01111",
               "10000",
               "10000",
               "01110",
               "00001",
               "00001",
               "11110",
               "00000"]

    elif c == 'T':
        return["11111",
               "00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "00000"]

    elif c == 'U':
        return["10001",
               "10001",
               "10001",
               "10001",
               "10001",
               "10001",
               "01110",
               "00000"]

    elif c == 'V':
        return["10001",
               "10001",
               "10001",
               "10001",
               "10001",
               "01010",
               "00100",
               "00000"]

    elif c == 'W':
        return["10001",
               "10001",
               "10001",
               "10101",
               "10101",
               "10101",
               "01010",
               "00000"]

    elif c == 'X':
        return["10001",
               "10001",
               "01010",
               "00100",
               "01010",
               "10001",
               "10001",
               "00000"]

    elif c == 'Y':
        return["10001",
               "10001",
               "10001",
               "01010",
               "00100",
               "00100",
               "00100",
               "00000"]

    elif c == 'Z':
        return["11111",
               "00001",
               "00010",
               "00100",
               "01000",
               "10000",
               "11111",
               "00000"]

    elif c == '[':
        return["01110",
               "01000",
               "01000",
               "01000",
               "01000",
               "01000",
               "01110",
               "00000"]

    elif c == '\\':
        return["00000",
               "10000",
               "01000",
               "00100",
               "00010",
               "00001",
               "00000",
               "00000"]

    elif c == ']':
        return["01110",
               "00010",
               "00010",
               "00010",
               "00010",
               "00010",
               "01110",
               "00000"]

    elif c == '^':
        return["00100",
               "01010",
               "10001",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000"]

    elif c == '_':
        return["00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "11111",
               "00000"]

    elif c == '`':
        return["01000",
               "00100",
               "00010",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000"]

    elif c == 'a':
        return["00000",
               "00000",
               "01110",
               "00001",
               "01111",
               "10001",
               "01111",
               "00000"]

    elif c == 'b':
        return["00000",
               "10000",
               "10000",
               "10110",
               "11001",
               "10001",
               "11110",
               "00000"]

    elif c == 'c':
        return["00000",
               "00000",
               "01110",
               "10000",
               "10000",
               "10001",
               "01110",
               "00000"]

    elif c == 'd':
        return["00000",
               "00001",
               "00001",
               "01101",
               "10011",
               "10001",
               "01111",
               "00000"]

    elif c == 'e':
        return["00000",
               "00000",
               "01110",
               "10001",
               "11111",
               "10000",
               "01110",
               "00000"]

    elif c == 'f':
        return["00000",
               "00110",
               "01001",
               "01000",
               "11100",
               "01000",
               "01000",
               "00000"]

    elif c == 'g':
        return["00000",
               "00000",
               "01111",
               "10001",
               "10001",
               "01111",
               "00001",
               "01110"]

    elif c == 'h':
        return["00000",
               "10000",
               "10000",
               "10110",
               "11001",
               "10001",
               "10001",
               "00000"]

    elif c == 'i':
        return["00000",
               "00100",
               "00000",
               "00100",
               "00100",
               "00100",
               "00100",
               "00000"]

    elif c == 'j':
        return["00000",
               "00010",
               "00000",
               "00110",
               "00010",
               "00010",
               "10010",
               "01100"]

    elif c == 'k':
        return["00000",
               "10000",
               "10000",
               "10100",
               "11000",
               "10100",
               "10010",
               "00000"]

    elif c == 'l':
        return["00000",
               "01100",
               "00100",
               "00100",
               "00100",
               "00100",
               "01110",
               "00000"]

    elif c == 'm':
        return["00000",
               "00000",
               "11010",
               "10101",
               "10101",
               "10001",
               "10001",
               "00000"]

    elif c == 'n':
        return["00000",
               "00000",
               "10110",
               "11001",
               "10001",
               "10001",
               "10001",
               "00000"]

    elif c == 'o':
        return["00000",
               "00000",
               "01110",
               "10001",
               "10001",
               "10001",
               "01110",
               "00000"]

    elif c == 'p':
        return["00000",
               "00000",
               "11110",
               "10001",
               "10001",
               "11110",
               "10000",
               "10000"]

    elif c == 'q':
        return["00000",
               "00000",
               "01111",
               "10001",
               "10011",
               "01101",
               "00001",
               "00001"]

    elif c == 'r':
        return["00000",
               "00000",
               "10110",
               "11001",
               "10000",
               "10000",
               "10000",
               "00000"]

    elif c == 's':
        return["00000",
               "00000",
               "01110",
               "10000",
               "01110",
               "00001",
               "11110",
               "00000"]

    elif c == 't':
        return["00000",
               "01000",
               "11100",
               "01000",
               "01000",
               "01001",
               "00110",
               "00000"]

    elif c == 'u':
        return["00000",
               "00000",
               "10001",
               "10001",
               "10001",
               "10011",
               "01101",
               "00000"]

    elif c == 'v':
        return["00000",
               "00000",
               "10001",
               "10001",
               "10001",
               "01010",
               "00100",
               "00000"]

    elif c == 'w':
        return["00000",
               "00000",
               "10001",
               "10001",
               "10101",
               "10101",
               "01010",
               "00000"]

    elif c == 'x':
        return["00000",
               "00000",
               "10001",
               "01010",
               "00100",
               "01010",
               "10001",
               "00000"]

    elif c == 'y':
        return["00000",
               "00000",
               "10001",
               "10001",
               "10001",
               "01111",
               "00001",
               "01110"]

    elif c == 'z':
        return["00000",
               "00000",
               "11111",
               "00010",
               "00100",
               "01000",
               "11111",
               "00000"]

    elif c == '{':
        return["00010",
               "00100",
               "00100",
               "01000",
               "00100",
               "00100",
               "00010",
               "00000"]

    elif c == '|':
        return["00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "00100",
               "00000"]

    elif c == '}':
        return["01000",
               "00100",
               "00100",
               "00010",
               "00100",
               "00100",
               "01000",
               "00000"]

    elif c == '~':
        return["00000",
               "00000",
               "01001",
               "10101",
               "10010",
               "00000",
               "00000",
               "00000"]
    # The function f ASCII #159
    elif c == 'ƒ':
        return["00010",
               "00100",
               "00100",
               "01110",
               "00100",
               "00100",
               "01000",
               "00000"]
    # Horizontal Ellipsis #Alt+0133  U+2026
    elif c == '…':
        return["00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "10101",
               "00000"]

    # A subsitute for the Haskell Logo >\=, ASCII #175
    elif c == '»':
        return["00000000000",
               "00000000000",
               "10010000000",
               "01001001111",
               "00100100000",
               "01001010011",
               "10010001000",
               "00000000000"]

    else:
        return ["00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "00000",
               "00111",
               "00000"]
