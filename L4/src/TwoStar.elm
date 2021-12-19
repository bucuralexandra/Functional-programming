module TwoStar exposing (..)

--problem 4

--problem 5
countriesWithCapital: List (String, String) -> (String -> Bool) -> List String
countriesWithCapital country funct = 
    List.map (\(a, _) -> a) (List.filter (\(_, b) -> funct b) country)

--problem 6
filterMap : (a -> Maybe b) -> List a -> List b
filterMap  functi l = 
    case l of 
        [] -> []
        x::xs -> case functi x of
                    Nothing -> filterMap functi xs
                    Just rez -> rez :: filterMap functi xs

--problem 7
any: ( a -> Bool) -> List a -> Bool
any p  l = List.foldl (\x y -> p x || y) False l

all p = List.foldl (\x y -> p x && y) True

--problem 8

chunks: Int -> List a -> List (List a)
chunks n l = 
    case l of 
        [] -> []
        _  :: xs -> (List.take n l) :: (chunks n (List.drop n l))

--problem 9