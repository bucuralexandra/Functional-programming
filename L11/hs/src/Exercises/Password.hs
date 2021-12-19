module Password where

available =  ['0'..'9'] ++  ['a'..'z'] ++ ['A'..'Z']
passwords :: Int -> [[Char]]
passwords n = sequenceA (replicate n available)