module Data.MSBoard.Expert
  ( movesWithOdds )
  where

import           Control.Monad
import           Data.Bifunctor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Ratio

import Data.MSBoard.Classes

lift2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2M f ma mb = join $ f <$> ma <*> mb

(?=) :: Eq a => a -> a -> Maybe a
a ?= a' = if a == a' then Just a else Nothing

-- | find all ways to split a list into two subsets of a particular size.
splitN :: Int -- ^ size of left list
        -> [a] -- ^ input list
        -> [([a],[a])] -- ^ every possible two-subset pair
splitN 0 as = [([],as)]
splitN k (a:as) | k > 0 = (first (a:) <$> splitN (k-1) as) ++ (second (a:) <$> splitN k as)
splitN _ _ = []

-- | Constraints
type Constraint = Map (Int,Int) Bool

matchConstraints :: [Constraint]-> Maybe Constraint
matchConstraints cs = sequence $ M.unionsWith (lift2M (?=)) ((Just <$>) <$> cs)

-- | Returns a list of every constraint that would satisfy a particular cell's
-- neighboring bomb count given the current board configuration
cellConstraints :: MSBoard board => board -> (Int,Int) -> [Constraint]
cellConstraints board ix = do
  (nbrsYes, nbrsNo) <- splitN (numNeighboringBombs board ix) (unpushedNeighbors board ix)
  return $ M.fromList $ zip nbrsYes (repeat True) ++ zip nbrsNo (repeat False)

-- Call cellConstraints on every pushed bomb that is adjacent to at least one
-- bomb. Then, pick a constraint from each constraint list, and unify the result
-- non-deterministically. Get a list of all the successfully unified constraint
-- lists. This list contains every possible scenario for the unpushed neighbors. From
-- this list, compute the odds for each unpushed neighbor.

-- | Compute all scenarios for the unpushed neighbors having bombs.
neighborScenarios :: MSBoard board => board -> [Constraint]
neighborScenarios board =
  let scenarios = sequence (cellConstraints board <$> allPushedNeighbors board)
  in  catMaybes (matchConstraints <$> scenarios)

-- | Given a board, returns a list of all the potential next neighbor-moves, along
-- with the odds that each move will not contain a mine.
movesWithOdds :: MSBoard board => board -> [((Int,Int), Ratio Int)]
movesWithOdds board = [ (ix, odds ix) | ix <- frontier ]
  where frontier = allUnpushedNeighbors board
        scenarios = neighborScenarios board
        odds ix = (fromIntegral $ getSum $ foldMap (Sum . count) scenarios) /
                  (fromIntegral $ length frontier)
          where count scenario = maybe 0 fromEnum $ M.lookup ix scenario

-- | Given a board, returns a list of all the best moves in descending order, along
-- with the odds that each move will not contain a mine.
bestMoves :: MSBoard board => board -> [((Int,Int), Ratio Int)]
bestMoves = undefined
