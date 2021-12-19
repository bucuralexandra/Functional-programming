module BmiCli where
import System.Environment (getArgs)
import GHC.Base (VecElem(Int16ElemRep))
main = do
    ws:hs:args <- getArgs 
    let w = read ws :: Double
    let h = read hs :: Double
    let bmi = w / (h*h)
    let bmis =  show bmi
    putStrLn $ "Your BMI is: " ++ bmis
