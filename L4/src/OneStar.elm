module OneStar exposing (..)

-- problem 1
enumerate : List a -> List (Int, a)
enumerate l = 
    let enumerateHelper index lx = 
            case lx of 
                [] -> []
                x::xs -> (index, x) :: enumerateHelper (index + 1) xs 
    in enumerateHelper 0 l

-- problem 2
repeat: Int -> a -> List a
repeat n elem = 
    if n == 0 then [] 
    else  elem :: repeat (n - 1) elem

-- problem 3

countVowels: String -> Int
countVowels word = 
    let 
        lx = String.toList word
    in 
    let
        vowelCount number l = 
            case l of
            [] -> number
            x::xs -> if List.member (x) ['a', 'e', 'i', 'o', 'u'] then 
                            vowelCount (number + 1) xs
                    else vowelCount number xs
    in 
        vowelCount 0 lx


isVowel: Char -> Bool
isVowel c = 
    if List.member(Char.toUpper(c)) ['A', 'E','I','O', 'U'] 
        then True
    else False

count : String -> Int
count w = 
    let
        listt = String.toList w
    in 
        List.length (List.filter isVowel listt)

