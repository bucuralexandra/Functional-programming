module ThreeStar where

breakToLines :: Int -> String -> [String]
breakToLines _ [] = []
breakToLines n l
  | n > 0 = (take n l) : (breakToLines n (drop n l))
  | otherwise = error "Negative or zero n"

formatLines :: [String] -> String
formatLines list = foldr (\a b-> a ++ if b=="" then b else "\n" ++ b) "" list