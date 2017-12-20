module Data.MSBoard.Expert
--  ( movesWithOdds )
  where

import           Control.Monad
import           Data.Bifunctor
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
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

(?^) :: Constraint -> Constraint -> Maybe Constraint
(?^) c d = sequence $ M.unionWith (lift2M (?=)) (Just <$> c) (Just <$> d)

(?~) :: Constraint -> Constraint -> Maybe Constraint
(?~) ctx c = const (c M.\\ ctx) <$> ctx ?^ c

reduceConstraints :: Constraint -> [Constraint] -> [Constraint]
reduceConstraints ctx cs = catMaybes $ (ctx ?~) <$> cs

-- | I couldn't come up with a good name for this. It's like sequence (and I was
-- using sequence before), but instead, it picks a constraint from the constraint set, then
-- first "reduces" ALL of the constraints in the remaining constraint sets. Here,
-- "reduce" means calling reduceConstraints with the chosen constraint to each of the
-- constraint sets.
collect :: [[Constraint]] -> [[Constraint]]
collect (cs:css) = do
  c' <- cs
  let reduced = reduceConstraints c' <$> css -- reduce the remaining constraint sets
  cs' <- collect reduced
  return $ c' : cs'
collect [] = [[]]

matchConstraints :: [Constraint]-> Maybe Constraint
matchConstraints cs = foldM (?^) M.empty cs

-- | Returns a list of every constraint that would satisfy a particular cell's
-- neighboring bomb count given the current board configuration
cellConstraints :: MSBoard board => board -> (Int,Int) -> [Constraint]
cellConstraints board ix = do
  (nbrsYes, nbrsNo) <- splitN (numNeighboringBombs board ix) (unpushedNeighbors board ix)
  return $ M.fromList $ zip nbrsYes (repeat True) ++ zip nbrsNo (repeat False)

-- | Compute all scenarios for the unpushed neighbors having bombs.
neighborScenarios :: MSBoard board => board -> [Constraint]
neighborScenarios board =
  let scenarios = collect (cellConstraints board <$> allPushedNeighbors board)
  in  catMaybes (matchConstraints <$> scenarios)

-- | Given a board, returns a list of all the potential next neighbor-moves, along
-- with the odds that each move will not contain a mine.
movesWithOdds :: MSBoard board => board -> [((Int,Int), Ratio Int)]
movesWithOdds board = [ (ix, odds ix) | ix <- frontier ]
  where frontier = allUnpushedNeighbors board
        scenarios = neighborScenarios board
        odds ix = (fromIntegral $ getSum $ foldMap (Sum . count) scenarios) /
                  (fromIntegral $ length scenarios)
          where count scenario = maybe 0 fromEnum $ M.lookup ix scenario

-- | Given a board, returns a list of all the best moves in descending order, along
-- with the odds that each move will not contain a mine.
bestMoves :: MSBoard board => board -> [((Int,Int), Ratio Int)]
bestMoves = undefined
