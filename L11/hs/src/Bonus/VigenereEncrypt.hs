module VigenereEncrypt where

import qualified Data.Char as Char
import System.Environment (getArgs)
import Vigenere


--encryptLetter letter = shift (Char.ord letter - Char.ord 'a')

encrypt :: String -> String -> String
encrypt key text = zipWith shift (multiplyKey (length text) key) text

-- >>> encrypt "battista" "a simple example"
-- "b lbuhee eqtuhee"

main :: IO ()
main = do
  key:args <- getArgs
  putStrLn "Input the message:"
  input <- getLine
  putStrLn $ encrypt key input