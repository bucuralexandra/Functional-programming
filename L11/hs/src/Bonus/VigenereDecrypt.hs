module VigenereDecrypt where

import qualified Data.Char as Char
import System.Environment (getArgs)
import Vigenere

-- >>> decrypt "giovan" "ZPSPNOXMOFAORMQDPUKZ"
-- "theunbreakablecipher"

-- >>> decrypt "battista" $ encrypt "battista" "a simple example"
-- "a simple example"
decrypt :: String -> String -> String
decrypt key text = zipWith shiftBack (multiplyKey (length text) key) text

main :: IO ()
main = do
  [key] <- getArgs
  putStrLn "Input your message"
  input <- getLine
  putStrLn $ decrypt key input
