
import Data.Time

split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == ' '  = "" : rest
   | c == '\n' = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = split cs

cleanser file start final size  | start >= size = final
                                | length ((split file)!!start) == 0 = final ++ cleanser file (succ start) final size
                                |  head ((split file)!!start) <= 'z' && head ((split file)!!start) >= 'a' && last ((split file)!!start) <= 'z' && last ((split file)!!start) >= 'a' = final ++ ((split file)!!start) ++ " " ++ cleanser file (succ start) final size
                                | otherwise = final ++ cleanser file (succ start) final size



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
      startF <- getCurrentTime
      file <- readFile "newCipher.txt"
      writeFile "words500" (replaceourkey (cleanser file 0 "" 500))
      stopF <- getCurrentTime
      print $ diffUTCTime stopF startF
