module TreeFunctor where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree  where
    fmap _ Nil = Nil
    fmap f (Node leftTree value rightTree) = Node (fmap f leftTree) (f value) (fmap f rightTree)