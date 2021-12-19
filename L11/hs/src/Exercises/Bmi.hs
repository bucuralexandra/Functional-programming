module Bmi where

main :: IO ()
main = do
    putStrLn "Enter your weight"
    ws <- getLine 
    let w =  read ws :: Double
    putStrLn "Enter your height in meters"
    hs <- getLine
    let h = read hs :: Double
    let bmi = w / (h*h)
    let bmis =  show bmi
    putStrLn $ "Your BMI is: " ++ bmis
        
