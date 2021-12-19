module Practice where

cycl :: [a] ->[a]
cycl l = l ++ cycl l

series :: [[Int]] 
series = [1] : (map (\l -> ((head l) + 1) :l) series)

-- [1] [1+1]: [1] = [2,1]
-- [2+1] : [2,1] = [3,2,1]

iter :: (a-> a) -> a -> [a]
iter func number = 
    number : iter func (func number)  

