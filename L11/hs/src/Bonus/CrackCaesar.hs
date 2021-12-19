module CrackCaesar where

import qualified Data.Char as Char
import System.Environment (getArgs)
import Data.List (isInfixOf, tails)


shift :: Int -> Char -> Char
shift d c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA + d) `mod` 26 + shA
      shA = Char.ord 'a'

encrypt :: Int -> String -> String
encrypt p c = map (shift p) c

decrypt :: Int -> String -> String
decrypt p c = map (shift (26 - p)) c

substr :: String -> String -> Bool
substr small large = 
    any (isInfixOf small) (tails large)

check :: Int -> String -> String -> (Int,Bool) 
check try message word =
  if try < 26 then
    if substr (encrypt try word) message == True then (try,True)
    else check (try + 1) message word
  else (-1,False)
-- >>> substr "question" "To be or not to be, that is the question"
-- True

-- >>> substr "that" "To be or not to be, that is the question"
-- True

main :: IO ()
main = do
  word:args <- getArgs
  putStrLn "Please input the cyphered text"
  cipheredText <- getLine 
  let (shiftNumber,answer) = check 0 cipheredText word
  let answerString = show answer
  let shiftNumberString = show shiftNumber
  let finalText = if shiftNumber < 0 then "Failed to crack cipher"
                  else "Found plaintext with key: " ++ shiftNumberString ++ "\n" ++ decrypt shiftNumber cipheredText
  putStrLn finalText
  
