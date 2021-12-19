module Three where

import Graph

import Data.List (find,nub)
import Data.Maybe (listToMaybe,mapMaybe)
import Debug.Trace (trace)

data SearchProb a =
  SearchProb { start :: a
             , expand :: a -> [a]
             , isDone :: a -> Bool }

type SearchAlgorithm a = SearchProb a -> Maybe a

dfs :: SearchAlgorithm a
dfs (SearchProb start expand isDone) = loop start where
  loop x | isDone x  = Just x
         | otherwise = listToMaybe $ mapMaybe loop (expand x)

bfs :: SearchAlgorithm a
bfs (SearchProb start expand isDone) = loop [start] where
  loop xs | any isDone xs = find isDone xs
          | otherwise     = loop (concatMap expand xs)