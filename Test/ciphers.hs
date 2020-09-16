-- ciphers.hs
--
-- Originally coded as a project for CMPS112, Comparative Programming Languages
-- A series of functions for handling shift and substitution ciphers.
-- Main function currently runs the last substitution cipher
--
-- by James Kuch

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord --(comparing)
import System.IO
import Control.Arrow --(&&&)
import Data.Set (Set)
import Data.Time
import qualified Data.Set as Set

-- Caeser cipher (aka Shift-cipher) section

--shifts an ASCII value, loops at char 128 (end of standard ASCII characters)
generalShift :: Int -> Char -> Char
generalShift i c = chr (((ord c) + i) `mod` 128)

--applies the above to an entire string/file.
lShift :: Int -> String -> String
lShift x s = map (generalShift x) s

--stand-alone shift algorithm for letters. Cases based on range, upper/lower-case, and looping alphabet.
shiftChar :: Int -> Char -> Char
shiftChar i c
    | value < ord 'A'                      = c                -- val < 'A' Out of range
    | shift <= ord 'Z'                     = chr shift        -- 'A' <= val + shift <= 'Z'
    | shift <= ord 'z' && value <= ord 'Z' = chr (shift - 26) -- val <= 'Z' && val + shift > 'Z'
    | value > ord 'Z' && value < ord 'a'   = c                -- 'Z' < val < 'a' Out of range
    | shift <= ord 'z' && value >= ord 'a' = chr shift        -- 'a' <= val + shift <= 'z'
    | value > ord 'z'                      = c                -- val > 'z' Out of range
    | shift > ord 'z'                      = chr (shift - 26) -- val <= 'z' && val + shift > 'z'
    where value = ord c
          shift = ((i `mod` 26) + (ord c))

--applies the special letters-only shift to a string/file
--useful to create jumbled input for the freq/brute-force solvers
aShift :: Int -> String -> String
aShift x s = map (shiftChar x) s

--attempts to solve a message by brute-forcing ASCII shifts
shiftSolverA :: String -> Int -> String -> String
shiftSolverA dict i file
    | a > 0.5 = (lShift i file) ++ "\n\nShift was ASCII by " ++ (show (i `mod` 128))
    | i > 129 = error "Simple shift cipher failed"
    | otherwise = shiftSolver dict (i+1) file
    where a = percentWords (Set.fromList (words (map makeUC dict))) (words (map makeUC (lShift i file)))

--same as the above, but letter-shifts
shiftSolverL :: String -> Int -> String -> String
shiftSolverL dict i file
    | a > 0.5   = (aShift i file) ++ "\n\nShift was Letters by " ++ (show (i `mod` 26))
    | i > 26    = []
    | otherwise = shiftSolver dict (i+1) file
    where a = percentWords (Set.fromList (words (map makeUC dict))) (words (map makeUC (aShift i file)))

--runs the letter-shift solver, if that fails, run the ASCII one
shiftSolver :: String -> Int -> String -> String
shiftSolver dict i file
   | a == []   = shiftSolverA dict i file
   | otherwise = a
   where a = shiftSolverL dict i file

--Frequency cipher
--tries to do a simple replace based on character frequency in a string/file with a given alphabet
--Some suggested alphabets (from http://en.wikipedia.org/wiki/Letter_frequency)
--"EOTHASINRDLUYMWFGCBPKVJQXZ", "ETAOINSHRDLUCMFWYPVBGKJQXZ" , "ETAONRISHDLFCMUGYPWBVKJXQZ"

--given two alphabets of equal length, does a translation on a char from one to the other
charReplace :: String -> String -> Char -> Char
charReplace alph freq c
    | not (length alph == length freq) = error "alphabets don't line up"
    | c `elem` freq = alph !! (fromJust (findIndex (\x -> x == c) freq))
    | otherwise = c

--takes all the letters in a string, sorts them, splits that into substrings of like characters
--converts the substrings to tuples based on (length of original substring, character), sorts them by length
--converts the list of tuples to a tuple of two lists (same data), takes only the list of characters
--then reverses them to get a list of most freq char to least freq all one string
findFreq :: String -> String
findFreq x = reverse (snd (unzip (sort (map (length &&& head) (group (sort x))))))

--general/full-ASCII version (an alphabet of all ascii characters based on frequency required
freqSolver :: String -> String -> String
freqSolver alph code = map (charReplace alph (findFreq code)) code

--used to force all letters in a file/string to be upper/lower case so t=T and can be counted as the same letter
makeUC :: Char -> Char
makeUC c
    | not (isLower c) = c
    | otherwise = chr ((ord c) - 32)

makeLC :: Char -> Char
makeLC x
    | not (isUpper x) = x
    | otherwise = chr((ord x) + 32)

--applies findFreq to letters only, forcing all characters to be upper case first for an accurate count
findFreqLetters :: String -> String
findFreqLetters x = findFreq (filter (isLetter) (map makeUC x))

--Letters-only version, for one-to-one alphabet substitution ciphers
freqSolverLetters :: String -> String -> String
freqSolverLetters alph code = map (charReplace (map makeUC (filter (isLetter) alph)) (findFreqLetters code)) (map makeUC code)


--brute-force substitution cipher solver, with a few cheats

--same thing as charReplace, but works with the alphabets (or key) binded to a tuple: ("ab","cd") is a=c, b=d.
charReplaceT :: (String, String) -> Char -> Char
charReplaceT ([],_) c = c
charReplaceT (_,[]) c = c
charReplaceT key c
    | not (length (fst key) == length (snd key)) = error "keys don't line up"
    | c `elem` (fst key) = (snd key) !! (fromJust (findIndex (\x -> x == c) (fst key)))
    | otherwise = c

--updates key tuple given a word and its translation
--i.e. key-("sp", "er") "qaa" "moo" = ("qasp", "moer")
updateKey :: (String, String) -> String -> String -> (String,String)
updateKey key [] _ = key
updateKey key _ [] = key
updateKey key x y
    | (head x) `elem` (fst key) = updateKey key (tail x) (tail y)
    | otherwise = updateKey ((head x):(fst key), (head y):(snd key)) (tail x) (tail y)

--given a set of words (used as a dictionary) and a list of words (body of text), give % of words in the list that are in the set- an accuracy check
percentWords :: Set String -> [String] -> Float
percentWords s x = (fromIntegral (length (filter (\w -> Set.member w s) x))) / (fromIntegral (length x))

--given dictionary, series all possible keys (restricted by length, known letters, etc.) brute-force check every key until a proper word is formed from given phrase
forceWord :: Set String -> [String] -> (String, String) -> String -> String -> String
forceWord dict perm key word cw
    | dict == Set.empty = word  --empty dictionary, no such thing as proper word
    | perm == []        = word  --all possible keys exhausted, return original phrase
    | word == []        = []    --empty phrase, empty word
    | Set.member cw dict  = cw  --a word in the dictionary was found, return it
    | otherwise = forceWord dict (tail perm) key word (map (charReplaceT (updateKey key (makeUniqueF word (fst key)) (head perm))) word)

--creates all possible permutations of combinations of length i of all letters.
--keys made = 26! / (26-i)!
makeKeys :: Int -> [String]
makeKeys 0 = [[]]
makeKeys i = concat (map permutations (combinations i "ABCDEFGHIJKLMNOPQRSTUVWXYZ") )

--same as above, filters out "known" letters given by parameter
--keys made (26-kl)! / (26-kl-i)! where kl = # of known letters
makeKeysF :: Int -> String -> [String]
makeKeysF 0 _ = [[]]
makeKeysF i s = concat (map permutations (combinations i (filter (\x -> not (x `elem` s)) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

--Cheat #1, 'e' being whatever the most common letter in the message is
findE :: String -> Char
findE s = head (findFreqLetters s)

--Cheat #2 "the" is an extremely common word that ends with 'e'
--take the first two letters of the most common word of length 3 that ends with 'e'
findThe :: String -> Char -> String
findThe [] _ = []
findThe s e = h:(head (drop 1 (head (filter (\xs -> length xs == 3 && last xs == e && head xs == h) (words s))))):[]
    where h = head (head (reverse (snd (unzip(sort (map (length &&& head) (group (sort (filter (\xs -> length xs == 3 && last xs == e) (words s))))))))))


--create list of all possible combinations given length and list of elements
--I take no credit for this function, found here: https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

--takes a word and returns a string of its unique letters, sorted
makeUnique :: String -> String
makeUnique [] = []
makeUnique x  = map (head) (group (sort x))

--same as the above, but also filters out known letters
makeUniqueF :: String -> String -> String
makeUniqueF [] _ = []
makeUniqueF x [] = makeUnique x
makeUniqueF x key = filter (\l -> not (l `elem` key)) (map (head) (group (sort x)))

--final build, takes dictionary, words left to check, key-tuple, and the original message
--tries to "build" a key based on given input, a frequency based shortcut (finding "the"), brute-force, and spellcheck to guess the remaining characters.
buildSolver :: Set String -> [String] -> (String, String) -> String -> String
buildSolver s xs ([],[]) file                              --initial run, empty key
    | ([e] == []) = file                                   --'e' not found, return file
    | (t == [])   = buildSolver s xs (e:[], "E") file      --"the" not found, proceed without it
    | otherwise   = buildSolver s xs (e:t, 'E':"TH") file  --"the" found,
    where e = findE file
          t = findThe file e
buildSolver s [] key file = map (charReplaceT key) file    --out of words to check, return what was found
buildSolver s x key file
    | s == Set.empty                   = a                                --empty dictionary, return based on given key
    | file == []                       = []                               --empty input, empty result
    | length (fst key) == 26           = a                                --all 26 letters found, return the full translation found
    | (percentWords s (words a)) > 0.5 = a                                --50%+ of the file seems accurate, return early- usually result can be easily pharsed by people at this point
    | (wk == (head x))                 = buildSolver s (tail x) key file  --brute-force didn't find result/word already accurate, continue
    | otherwise                        = buildSolver s (tail x) (updateKey key (head x) wk) file --update the key based on the result found from brute force, continue
    where a  = map (charReplaceT key) file
          wk = forceWord s (makeKeysF (length (makeUniqueF (head x) (fst key))) (snd key)) key (head x) (head x)

--ugly, hardcoded IO. "wordsEn.txt" being the dictionary and "test.txt" being the file to test the cipher on
main = do
    dict <- readFile "newsmall"
    file <- readFile "words"

--Commented out code here is all perfectly functional, uncomment to test each solver. Each one is also timed.
{-
--tests the shift funciton, shifts all characters 14 ASCII places
    startS <- getCurrentTime
    writeFile "shiftedTest.txt" (aShift 14 file)
    stopS <- getCurrentTime
    print $ diffUTCTime stopS startS
-}
{-
--tests the naive frequency based solver on a test file
    startF <- getCurrentTime
    writeFile "fTest.txt" (buildSolver dict file ([],[]) file)
    stopF <- getCurrentTime
    print $ diffUTCTime stopF startF
-}
{-
--tests the Caesar cipher solver (full ASCII range)
    start <- getCurrentTime
    writeFile "shiftTestSolvedA.txt" (shiftSolverA dict 0 (lShift 127 file))
    stop <- getCurrentTime
    print $ diffUTCTime stop start
-}
{-
--tests the Caesar cipher solver (Letters only, A-Z range)
    startA <- getCurrentTime
    writeFile "shiftTestSolvedL.txt" (shiftSolverL dict 0 (aShift 25 file))
    stopA <- getCurrentTime
    print $ diffUTCTime stopA startA
-}

--tests the general substitution cipher solver on a shifted (letters only) test file, for convenience, all letters are made uppercase
    startB <- getCurrentTime
    writeFile "solvedTestGS.txt" (buildSolver (Set.fromList (words (map makeUC dict))) (words (map makeUC (aShift 13 file))) ([],[]) (map makeUC (aShift 13 file)) )
    stopB <- getCurrentTime
    print $ diffUTCTime stopB startB
