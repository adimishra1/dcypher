README
James Kuch

-ciphers.hs
The main program, tested in GHCi with (all files in ~\Haskell Platform\2014.2.0.0\winghci)
:l ciphers.hs

-ciphers.hs
The main program, contains a series of simple cipher related functions including:
-A couple Caeser ciphers (aka a shift cipher) and solvers for both.
 -One version considers all ASCII characters and the other only considers letters)

-A frequency based substitution cipher solver that takes each character's frequency
 and assumes a solution based on a preset alphanumeric order

-A brute force based substitution cipher solver that uses a few tricks to find out
 some of the more common letters, then tries combinations for all the other letters
 until it finds a word- it then takes the letters of that word and adds them to a list
 of known letters, thereby severely reducing the number of brute-force combinations to check.
 This process repeats until all 26 letters are found, or the file passes a spellcheck of at least 50% accuracy
 (Results are usually still error prone, but usually to the point physical inspection can reveal the solution)

-A spellchecker used to check the solutions of the above ciphers for accuracy
 -dictionary for the spellchecker provided below

-main, consists of several commented out benchmarks tests- they all work though.
 The last cipher is uncommented for use.

for details of helper functions, please see comments.

-wordsEn.txt
An extensive list of words used as a makeshift dictionary for the brute force algorithms. Source & stats below.
http://www-01.sil.org/linguistics/wordlists/english/
-109582 words, -935172 characters


281‡(8 ;48 5†¶8*; ‡1 ;48 -:28( ]‡(0†, 61 )‡98‡*8 ]5*;8† ;‡ 16*† ‡?; 52‡?; 5 -‡?*;(: ;48: ]6)48† ;‡ ¶6)6;, ;48: ]8*; ;‡ 062(5(68) 5*† 2‡‡7 );‡(8); ;48: (85† ;(5¶80 3?6†8) 5*† (8)85(-48† 85-4 5*† 8¶8(: †8;560 52‡?; ;48 -‡?*;(:. ;48: ]8*; ‡?; 5*† )5] ;48 ]‡(0†, $?8);6‡*8† 6;, ;(68† ;‡ ?*†8();5*† 6;. ;48: *8¶8( 3‡‡308† 6;, ;(:6*3 ;‡ -‡*)?98 500 )‡(;) ‡1 †8;560) 6* 5 ).5* ‡1 15 96*?;8).
6*†88†, (85†6*3 ;48 52);(5-; ‡1 5 (8)85(-4 .5.8( ;‡ -6;8 6; 6) *‡; 8*‡?34. ‡*06*8 (8)85(-4 – 8¶8* 61 :‡? ?)8 ‡*06*8 2‡‡7), 6) *‡; 8*‡?34. :‡? *88† ;‡ 3‡ ‡?;. :‡?( .4:)6-50 )801 *88†) ;‡ 8¢.8(68*-8 ;48 (850 ]‡(0†, ;48 7*‡]08†38. ;48(8 )4‡?0† 28 6998()6‡*. 5 305*-8 5; ;48 ](6;;8* ]‡(† ;4(‡?34 5 )-(88* 6) *‡; 8*‡?34.
;48 6*;8(*8; 6) .(‡1‡?*†0: 05-76*3 ]48* 6; -‡98) ;‡ -5;8(6*3 ;‡ ;48 3(‡];4 ‡1 4?95* 286*3). 6; †‡8) *‡;46*3 1‡( ;48 4?95* 6*;8008-; ‡( ;48 4?95* ).6(6;; 500 6; †‡8) 6) .(‡¶6†8 ?) ]6;4 *?98(‡?) ;:.8) ‡1 6*1‡(95;6‡*. ¶8(: 18] 9634; (8506)8 6;, 2?; ;46) 480.1?0 ;‡‡0 ;45; ]8 -500 ;48 6*;8(*8; 6) 5 -?()8 6* )8¶8(50 ]5:). 500 ;45; ;48 6*;8(*8; 45) 8¶8( †‡*8 6) );53*5;8 ;48 96*†, 5*† †?00 ;48 )‡?0. 5*† 6 ;46*7 6;’) 52‡?; ;698 ]8 );‡..8† (80:6*3 ‡* 6;.
