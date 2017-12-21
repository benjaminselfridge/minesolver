module Minesweeper where

import           Control.Monad
import           Data.Bifunctor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)

lift2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2M f ma mb = join $ f <$> ma <*> mb

(?=) :: Eq a => a -> a -> Maybe a
a ?= a' = if a == a' then Just a else Nothing

-- | Constraints
type Constraint = Map Int Bool

matchConstraints :: [Constraint] -> Maybe Constraint
matchConstraints cs = sequenceA $ M.unionsWith (lift2M (?=)) ((Just <$>) <$> cs)

-- | Cells

data Cell = Cell { cellNbrs  :: [Int]
                 , cellBombs :: Int
                 }
  deriving Show

-- | find all ways to split a list into two subsets of a particular size.
splitN :: Int -- ^ size of left list
        -> [a] -- ^ input list
        -> [([a],[a])] -- ^ every possible two-subset pair
splitN 0 as = [([],as)]
splitN k (a:as) | k > 0 = (first (a:) <$> splitN (k-1) as) ++ (second (a:) <$> splitN k as)
splitN _ _ = []

-- | From a cell, generate all (n choose k) constraints, where n is the number of
-- neighbors and k is the number of bombs.
cellConstraint :: Cell -> [Constraint]
cellConstraint (Cell nbrs bombs) = do
  (nbrsYes, nbrsNo) <- splitN bombs nbrs
  return $ M.fromList $ zip nbrsYes (repeat True) ++ zip nbrsNo (repeat False)

ex1 = [ Cell [2,7] 1
      , Cell [10,11] 2
      , Cell [2,7,10,11,12] 3
      ]



