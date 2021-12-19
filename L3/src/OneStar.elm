module OneStar exposing (..)

q = 1
-- exercise 1
safeDiv : Int -> Int -> Maybe Int
safeDiv a b = 
    case b of
    0 -> Nothing
    _ -> Just (a // b)

-- exercise 2

countFromTo : Int -> Int -> List Int
countFromTo fromm tooo = if fromm >= tooo then []
    else fromm :: countFromTo (fromm + 1) tooo

-- Tail recursive list length
len : List a -> Int
len l = 
        let
         lenTR lx acc = 
                case lx of
                 [] -> acc
                 _::xs -> lenTR xs (acc + 1)
     in
            lenTR l 0

-- Exercise 3
last : List a -> Maybe a
last l =
 case l of
    [] -> Nothing
    x::[] -> Just x
    _::xs -> last xs

-- Ex. 3.5.4
-- indexList i l
-- Returns i-th element in list
indexList : Int -> List a -> Maybe a
indexList i l = 
    case l of
        [] -> Nothing
        x::xs -> if i == 0 then Just x else indexList (i - 1) xs


-- Ex. 3.5.5
-- fibs start end
-- Fibonacci numbers indexed [start, end)
-- fibs 0 3 => [1, 1, 2]
-- fibs 3 5 => [3 5]
fib : Int -> Int
fib n = if (n == 0) || (n == 1) then 1 else (fib (n - 1) + fib (n - 2))

fibs : Int -> Int -> List Int
fibs start end = if start == end then [] else (fib start)::(fibs (start + 1) end)

-- 3.5.6

-- fibsIndex : Int -> Int -> List (Int, Int)
-- fibsIndex start end =
--     if start == end then []
--     else 
--         let 
--             index i st = 
--                 case st of
--                     0 -> (0, 1)
--                     1 -> (1, 1)
--                     _ -> (index (i + 1 ) (st - 1) + index (i + 1) (st - 2)) 
--         in 

reverse : List a -> List a
reverse l =
    let reverseAcc lx acc = case lx of
                            [] -> acc
                            x::xs -> reverseAcc xs (x::acc)
    in
        reverseAcc l []

fibStartEndTuple: Int->Int ->List (Int, Int)
fibStartEndTuple start end =
    let
        fibStartEndTupleHelper myStart myEnd myList =
            if(myStart == myEnd) then myList
            else fibStartEndTupleHelper (myStart + 1) myEnd (((myStart), (fib myStart)):: myList)
    in
        reverse(fibStartEndTupleHelper start end [])