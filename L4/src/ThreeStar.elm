module ThreeStar exposing (..)

--pb1 -> enumeraye cu foldl

enumerate: List a -> List (a, Int)
enumerate l = 
    List.foldl (::) [] (List.foldl (\x a -> (x, (List.length a)) :: a) [] l)

--pb 2
collect : List (Result err ok) -> Result err (List ok)
collect ans = 
    let helper lx acc = case lx of 
                            [] -> Ok acc
                            x :: xs -> case x of
                                        Err err -> Err err
                                        Ok value -> helper xs (value :: acc)
    in helper ans []
