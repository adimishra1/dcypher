import Data.Char
import Data.List
import Data.Time
import Data.Function (on)
import Data.List (sortBy)

nayimasti word dict key = key++[hammingReplace dict word key]
nayawordbyword file key dict start | start >= (length file) = key
                                   | numberofunknowns (file!!start) == 1 && total (file!!start) dict == 1 = nayimasti (file!!start) dict key ++ nayawordbyword file key dict (succ start)
                                   | otherwise = nayawordbyword file key dict (succ start)
n_on_the_rocks [] = []
n_on_the_rocks (x:xs) | x == 'i' = (head xs):(n_on_the_rocks xs)
                      | otherwise = n_on_the_rocks xs

patternmatch "" "" = True
patternmatch word "" = False
patternmatch "" word = False
patternmatch word1 word2 | numberofunknowns word1 /= hammingDistance word1 word2 = False
                         | (head word1) /= (head word2) && ord (head word1) > 96 && ord(head word1) < 123 = False
                         | otherwise = patternmatch (tail word1) (tail word2)

total [] dict = 0
total word [] = 0
total word dict | length word == length (head dict) && patternmatch word (head dict) = total word (tail dict) + 1
                | otherwise = total word (tail dict)

numberofunknowns [] = 0
numberofunknowns (x:xs) | (ord x > 96) && (ord x < 123) = numberofunknowns xs
                        | otherwise = numberofunknowns xs + 1

removeline :: String -> String
removeline [] = []
removeline (x:xs) | x==',' = removeline xs
                  | x=='¢' = ' ' : removeline xs
                  | otherwise = x : removeline xs


split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == ' '  = "" : rest
   | c == '\n' = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = split cs




count [] dict = []
count (x:xs) dict | isInfixOf x dict = x : count xs dict
                  | otherwise = count xs dict

frequencyOfElt :: (Eq a) => [a] -> [(a,Int)]
frequencyOfElt xs = countElt (unique xs) xs     -- change the result type
  where
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)


    countElt ref target =   -- Code it Point-Free Style  (your original idea)
      zip
        ref $               -- your original type would need (map (:[]) ref) here
        map length $
          zipWith ($)       -- ((filter . (==)) c) === (filter (== c))
            (zipWith ($) (repeat (filter . (==))) ref)
            (repeat target)

isdone char [] = True
isdone char (x:xs) | char==x = False
                   | otherwise = isdone char xs

-- hrep word1 word2 key | (head word1) /= (head word2) && isdone (head word2) (snd (unzip key)) = ((head word1),(head word2))
--                      | otherwise = hrep (tail word1) (tail word2) key

hrep word1 word2 key | (head word1) /= (head word2)  = ((head word1),(head word2))
                     | otherwise = hrep (tail word1) (tail word2) key
-- hrepnew word1 word2 key | (head word1) /= (head word2) = ((head word1),(head word2))
                        -- | otherwise = hrep (tail word1) (tail word2) key


advanvedhrep [] [] key = key
advancedhrep [] word1 key = key
advancedhrep word2 [] key = key
advancedhrep word1 word2 key | (head word1) /= (head word2) && isdone (head word2) (snd (unzip key)) =  [((head word1),(head word2))] ++ advancedhrep (tail word1) (tail word2) key
                             |  otherwise = advancedhrep (tail word1) (tail word2) key

hammingDistance la lb = length (filter id (zipWith (/=) la lb))

hamming [] word = 0
hamming dict word | hammingDistance (head dict) word == 1 && length (head dict) == length word = hamming (tail dict) word + 1
                  | otherwise = hamming (tail dict) word

hammingReplace [" "] word key = (' ',' ')
hammingReplace dict " " key = (' ',' ')
hammingReplace dict word key | hammingDistance (head dict) word == 1 && length (head dict) == length word = hrep word (head dict) key
                             | otherwise = hammingReplace (tail dict) word key

advancedHammingReplace [" "] word key = key
advancedHammingReplace dict " " key = key
advancedhammingReplace dict word key | hammingDistance (head dict) word==numberofunknowns word && length (head dict) == length word = advancedhrep word (head dict) key
                                     | otherwise = advancedhammingReplace (tail dict) word key


match x [] = x
match x key | x == fst (head key) = snd (head key)
            | otherwise = match x (tail key)

replaceWithKey :: [(Char,Char)] -> [Char] -> [Char]
replaceWithKey key [] = []
replaceWithKey key (x:xs) | ord x < 33 = ' ' : replaceWithKey key xs
                          | otherwise = (match x key) : replaceWithKey key xs

freq xs = filter (\x -> (ord (fst x))>32) (frequencyOfElt xs)

startkey key file = key++[(fst (head (reverse (sortBy (compare `on` snd) (freq file)))),'e')]++[(fst (head (tail (reverse (sortBy (compare `on` snd) (freq file))))),'t')]

-- afinder key file = key++[(head (head (filter (\x -> (length x)==1) (split file))),'a')]
afinder key file = key
ofinder key file = key ++ [(head (tail (head (filter (\x -> (head x=='t') && (length x==2)) (split (replaceWithKey key file))))),'o')]
-- ofinder key file = key
hfinder key file = key ++ [(head (tail (head (filter (\x -> (head x=='t') && (length x==3) && (last x=='e')) (split (replaceWithKey key file))))),'h')]
-- hfinder key file = key
-- ifinder key file = key++[(head (head (filter (\x -> (head x/='a') && (length x==2) && (last x=='t')) (split (replaceWithKey key file)))),'i')]
ifinder key file = key
-- nfinder key file = key++[(fst (head (reverse (sortBy (compare `on` snd) (freq (n_on_the_rocks (replaceWithKey key file)))))),'n')]
nfinder key file = key
-- rfinder key file = key++[(last(init(head(filter (\x->(head x=='t')&&(length x==5)&&(head (tail x)=='h')&&(last x == 'e')&&(head (tail ( tail x))=='e')&&(last (init x)/='s')) (split (replaceWithKey key file))))), 'r')]
rfinder key file = key
--sfinder key file = key++[(last(head(filter (\x->(head x=='t')&&(length x==4)&&(head (tail x)=='h')&&(last x/= 'n')&&(last (init x)=='i')) (split (replaceWithKey key file)))),'s')]

--keymaker [] dict key start = key
keymaker3 file key dict start | (start>=length (split file)) = key
                              | numberofunknowns ((split file)!!start) == 1 && length ((split file)!!start) == 3 &&  hamming (filter (\x -> length x==3) dict) ((split file)!!start) == 1 = key++[hammingReplace dict ((split file)!!start) key] ++ (keymaker3 file key dict (succ start))
                              | otherwise = keymaker3 file key dict (succ start)

keymaker4 file key dict start | (start>=length (split file)) = key
                              | numberofunknowns ((split file)!!start) == 1 && length ((split file)!!start) == 4 &&  hamming (filter (\x -> length x==4) dict) ((split file)!!start) == 1 = key++[hammingReplace dict ((split file)!!start) key] ++ (keymaker4 file key dict (succ start))
                              | otherwise = keymaker4 file key dict (succ start)

keymaker5 file key dict start | (start>=length (split file)) = key
                              | numberofunknowns ((split file)!!start) == 1 && length ((split file)!!start) == 5 &&  hamming (filter (\x -> length x==5) dict) ((split file)!!start) == 1 = key++[hammingReplace dict ((split file)!!start) key] ++ (keymaker5 file key dict (succ start))
                              | otherwise = keymaker5 file key dict (succ start)



wordbyword file key dict start final | start >= length (split file) = final
                                     | numberofunknowns ((split file)!!start) >0 && total ((split file)!!start) dict == 1 = advancedhammingReplace (filter (\x -> length x == length ((split file)!!start) ) dict) ((split file)!!start) key ++ final
                                     | otherwise = wordbyword file key dict (succ start) final


searchfullstop oldf file  start  | start >= length (split file) = last((split file)!!start)
                                 | last((split oldf)!!start) == '.' = last((split file)!!start)
                                 | otherwise = searchfullstop oldf file (succ start)

fullstopcheck:: String -> Int -> String -> [Char] -> [Char] -> [Char]
fullstopcheck file start oldf dict final | start >= (length (split file) - 1) = final
                                         | (length((split file)!!start) /= 0) && (last ((split file)!!start) == last(last(init (split file)))) && (isInfixOf ((split file)!!start) dict == False) = final++(init ((split file)!!start))++". "++ fullstopcheck file (succ start) oldf dict final
                                         | otherwise = final ++ ((split file)!!start) ++ " " ++ fullstopcheck file (succ start) oldf dict final

finalkey file dict = (wordbyword (replaceWithKey (keymaker5 (replaceWithKey (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) file) (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(' ',' ')] file) file) file) file) file) file) file) (split dict) 0) (split dict) 0) file) (keymaker5 (replaceWithKey (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) file) (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) (split dict) 0) (split dict) 0 (key1 file dict))

-- finalkey file dict = (wordbyword file (key2 file dict) (split dict) 0 (key2 ))


-- last(last(split file))

key1 file dict = (keymaker5 (replaceWithKey (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) file) (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) (split dict) 0)

main = do
      dict <- readFile "dictionary"
      file <- readFile "GoldBugCipherSpecialCharecters.txt"

      startF <- getCurrentTime
      writeFile "tmp1.txt" (replaceWithKey (finalkey file dict) file)

      filetemp <- readFile "tmp1.txt"
      writeFile "tmp2.txt" (replaceWithKey (nayawordbyword (split filetemp) [(',',',')] (split dict) 0) filetemp)
      filetemp1 <- readFile "tmp2.txt"
      writeFile "tmp1.txt" (replaceWithKey (nayawordbyword (split filetemp1) [(',',',')] (split dict) 0) filetemp1)
      filetemp2 <- readFile "tmp1.txt"
      writeFile "tmp2.txt" (replaceWithKey (nayawordbyword (split filetemp2) [(',',',')] (split dict) 0) filetemp2)

      filetemp2 <- readFile "tmp2.txt"
      writeFile "DicipheredTextSpecialCharecters.txt" (fullstopcheck filetemp2 0 file dict "")
      stopF <- getCurrentTime
      print $ diffUTCTime stopF startF
