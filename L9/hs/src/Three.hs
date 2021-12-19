module Three where
import System.Win32 (COORD(x))

nextSqrt :: Float -> Float -> Float  
nextSqrt x y = (x / y + y)/ 2


iter :: (a -> a) -> a -> [a]
iter func number = 
    number : iter func (func number)  

approxS :: Float -> [Float]
approxS x = [1]

goodEnough :: Float -> (Float, Float) -> Bool 
goodEnough eps (x,y) = abs x-y < eps

genApprox :: Float -> Float -> Float 
genApprox eps x = 0


approxSqrt :: Float -> Float -> Float 
approxSqrt eps x = genApprox eps x