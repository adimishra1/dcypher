import Data.Char
import Data.List
import Data.Time
import Data.Function (on)
import Data.List (sortBy)


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



split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == ' '  = "" : rest
   | c == '\n' = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = split cs


searchfullstop key | fst (head key) == '.' = snd (head key)
                   | otherwise = searchfullstop (tail key)


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

hrep word1 word2 key | (head word1) /= (head word2) && isdone (head word2) (snd (unzip key)) = ((head word1),(head word2))
                     | otherwise = hrep (tail word1) (tail word2) key

advancedhrep [] [] key = key
advancedhrep [] word1 key = key
advancedhrep word2 [] key = key
advancedhrep word1 word2 key | (head word1) /= (head word2) && isdone (head word2) (snd (unzip key)) = key ++ [((head word1),(head word2))] ++ advancedhrep (tail word1) (tail word2) key
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

startkey key file = key++[(fst (head (reverse (sortBy (compare `on` snd) (freq file)))),'e')]++[(fst (head (tail (reverse (sortBy (compare `on` snd) (freq file))))),'t')] ++ [(fst (head (tail (tail (reverse (sortBy (compare `on` snd) (freq file)))))),'a')] ++ [(fst (head (tail (tail (tail (reverse (sortBy (compare `on` snd) (freq file))))))),'o')] ++[(fst (head (tail (tail (tail (tail (reverse (sortBy (compare `on` snd) (freq file)))))))),'i')]++[(fst (head (tail (tail (tail (tail (tail (reverse (sortBy (compare `on` snd) (freq file))))))))),'n')]

afinder key file = key

ofinder key file = key

hfinder key file = key

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



wordbyword file key dict start | start >= length (split file) = key
                               | numberofunknowns ((split file)!!start) >0 && total ((split file)!!start) dict == 1 = advancedhammingReplace (filter (\x -> length x == length ((split file)!!start) ) dict) ((split file)!!start) key ++ wordbyword file key dict (succ start)
                               | otherwise = wordbyword file key dict (succ start)

fullstopcheck:: String -> Int -> [(Char, Char)] -> [Char] -> [Char] -> [Char]
fullstopcheck file start key dict final | start >= (length (split file)-1) = final
                                        | last ((split file)!!start) == searchfullstop key && isInfixOf ((split file)!!start) dict == False = final++(init ((split file)!!start))++". "++ fullstopcheck file (succ start) key dict final
                                        | otherwise = final ++ ((split file)!!start) ++ " " ++ fullstopcheck file (succ start) key dict final

finalkey file dict = (wordbyword (replaceWithKey (keymaker5 (replaceWithKey (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) file) (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(' ',' ')] file) file) file) file) file) file) file) (split dict) 0) (split dict) 0) file) (keymaker5 (replaceWithKey (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) file) (keymaker4 (replaceWithKey (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) file) (rfinder (nfinder (ifinder (hfinder (ofinder (afinder (startkey [(',',',')] file) file) file) file) file) file) file) (split dict) 0) (split dict) 0) (split dict) 0)


cleanser file start final | start >= length (split file) = final
                          | length ((split file)!!start) == 0 = final ++ cleanser file (succ start) final
                          | head ((split file)!!start) <= 'z' && head ((split file)!!start) >= 'a' && last ((split file)!!start) <= 'z' && last ((split file)!!start) >= 'a' = final ++ ((split file)!!start) ++ " " ++ cleanser file (succ start) final
                          | otherwise = final ++ cleanser file (succ start) final



replaceourkey [] = []
replaceourkey (x:xs) | x=='a' = '!' : replaceourkey xs
                     | x=='b' = '#' : replaceourkey xs
                     | x=='c' = '$' : replaceourkey xs
                     | x=='d' = '%' : replaceourkey xs
                     | x=='e' = '&' : replaceourkey xs
                     | x=='f' = '(' : replaceourkey xs
                     | x=='g' = ')' : replaceourkey xs
                     | x=='h' = '*' : replaceourkey xs
                     | x=='i' = '+' : replaceourkey xs
                     | x=='j' = '-' : replaceourkey xs
                     | x=='k' = '.' : replaceourkey xs
                     | x=='l' = '0' : replaceourkey xs
                     | x=='m' = '1' : replaceourkey xs
                     | x=='n' = '2' : replaceourkey xs
                     | x=='o' = '3' : replaceourkey xs
                     | x=='p' = '4' : replaceourkey xs
                     | x=='q' = '5' : replaceourkey xs
                     | x=='r' = '6' : replaceourkey xs
                     | x=='s' = '7' : replaceourkey xs
                     | x=='t' = '8' : replaceourkey xs
                     | x=='u' = '9' : replaceourkey xs
                     | x=='v' = ':' : replaceourkey xs
                     | x=='w' = ';' : replaceourkey xs
                     | x=='x' = '<' : replaceourkey xs
                     | x=='y' = '=' : replaceourkey xs
                     | x=='z' = '>' : replaceourkey xs
                     | otherwise = x : replaceourkey xs



main = do
      dict <- readFile "newsmall"
      file <- readFile "newCipher.txt"

      startF <- getCurrentTime
      -- writeFile "words500" (replaceourkey (cleanser file 0 ""))

      writeFile "fTest2.txt" (replaceWithKey (finalkey file dict) file)
      filetemp <- readFile "fTest2.txt"
      writeFile "ouranswer.txt" (fullstopcheck filetemp 0 (finalkey file dict) dict "")
      stopF <- getCurrentTime
      print $ diffUTCTime stopF startF
