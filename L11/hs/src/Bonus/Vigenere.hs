module Vigenere where

import qualified Data.Char as Char

formatLines :: [String] -> String
formatLines = foldr (\a b-> a ++ if b=="" then b else b) ""

--makes the key as long as the word to be encoded
multiplyKey :: Int -> String -> String
multiplyKey len key = take len (formatLines (replicate  len key))

shift :: Char -> Char -> Char
shift keyLetter c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA + (Char.ord keyLetter - shA) ) `mod` 26 + shA
      shA = Char.ord 'a'

shiftBack :: Char -> Char -> Char
shiftBack keyLetter c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA - (Char.ord keyLetter - shA))  `mod` 26 + shA
      shA = Char.ord 'a'