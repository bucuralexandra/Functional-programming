module ExercitiiAle exposing (..)
import List exposing (filter)
import List exposing (length)
import Lists exposing (map)

all:  (a -> Bool) -> List a -> Bool
all func l= 
    if (length <| filter (\x -> x == False) <| map( func ) <| l ) > 0 then False
    else True
    
any:  (a -> Bool) -> List a -> Bool
any func l= 
    if (length <| filter (\x -> x == True) <| map( func ) <| l ) > 0 then True
    else False