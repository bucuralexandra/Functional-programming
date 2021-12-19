module Practice where
import YesNo (YesNo)
import Container (Container(..))

-- Exercise 10.9.1
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


-- Exercise 10.9.2
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True


-- Exercise 10.9.3
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

instance Container Tree where
    hasElem Nil _ = False
    hasElem (Node leftTree v rightTree) e
        | v == e = True 
        | otherwise = hasElem leftTree e || hasElem rightTree e

    nrElems Nil = 0
    nrElems (Node leftTree _ rightTree) = 1 + nrElems leftTree + nrElems rightTree


listTree = 
    Node
        (Node (Node Nil [12, 13] Nil) [5,5,6] Nil)
    [1,2,3]
        (Node Nil [6,1] (Node (Node Nil [] Nil) [] (Node Nil [1] Nil)))