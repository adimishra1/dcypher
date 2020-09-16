# Dcypher

Dcypher is a decoder for decoding text encrypted with Gold-Bug cipher in Haskell with accuracy of 92% over the hidden test set. Used various heuristics like frequency, bigram and trigram analysis and hamming distance comparison to improve both running time and accuracy of the algorithm.

## Encrypted Text with normal Characters

1. Add the encrypted text by Gold-Bug Cipher in file GoldBugCipher.txt
2. Compile and run Dcypher.hs file
3. Output will be in the file DicipheredText.txt

```
ghc Dcypher.hs
./Dcypher
```

## Encrypted Text with Special Characters

1. Add the encrypted text by Gold-Bug Cipher in file GoldBugCipherSpecialCharacters.txt all text here must be of non-alphabetical type.
2. Compile and run DcypherWithSpecialCharacters.hs file
3. Output will be in the file DicipheredTextWithSpecialCharacters.txt

```
ghc DcypherWithSpecialCharacters.hs
./Dcypher
```
