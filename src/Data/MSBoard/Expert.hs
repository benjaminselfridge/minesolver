-- | This module exports a single function, movesWithOdds, that produces an alist
-- mapping individual cell indices to the odds (as a Rational) that the cell has a
-- bomb. The odds are only calculated based on what is visible to the user - they do
-- not use any information about a cell that hasn't already been revealed by clicking
-- on it (or one of its neighbors).

module Data.MSBoard.Expert
  ( movesWithOdds
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Ratio

import Data.MSBoard.Classes

-- | find all ways to split a list into two subsets of a particular size.
splitN :: Int -- ^ size of left list
        -> [a] -- ^ input list
        -> [([a],[a])] -- ^ every possible two-subset pair
splitN 0 as = [([],as)]
splitN k (a:as) | k > 0 = (first (a:) <$> splitN (k-1) as) ++ (second (a:) <$> splitN k as)
splitN _ _ = []

-- Constraints

-- | Two-argument variant of bind.
-- Given a function that takes two pure arguments and produces a monadic value, we
-- get a new function that takes two monadic arguments and produces a monadic value.
lift2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
lift2M f ma mb = join $ f <$> ma <*> mb

-- | If the two arguments are equal, return them/it -- otherwise, return
-- Nothing. This is for convenience in defining (?^).
(?=) :: Eq a => a -> a -> Maybe a
a ?= a' = if a == a' then Just a else Nothing

-- | a partial assignment of truth values to cells
-- Here, `True` means "has a bomb." The idea is that every pushed cell that
-- has a nonzero number of neighboring bombs imposes one of a set of potential
-- constraints -- for instance, if a cell has two neighboring bombs and is adjacent
-- to four unpushed cells, we know that two out of those four
type Constraint = Map (Int,Int) Bool

-- | Unify two constraints into a single constraint, if they don't conflict.
(?^) :: Constraint -> Constraint -> Maybe Constraint
c ?^ d = sequence $ M.unionWith (lift2M (?=)) (Just <$> c) (Just <$> d)

-- | Ensure that two constraints agree, and then return the second one minus the keys
-- of the first one. This is mainly useful for defining the `reduceConstraints`
-- function.
(?~) :: Constraint -> Constraint -> Maybe Constraint
ctx ?~ c = const (c M.\\ ctx) <$> ctx ?^ c

-- | Given a "global" or "context" constraint representing a set of assumptions, and
-- another set of potential additional constraints, return a list consisting of only
-- those constraints that agree with the global one (ctx). Additionally, those
-- constraints that survive this process also get pruned of all the keys that are
-- already in the context; this eliminates redundancy in later computations.
reduceConstraints :: Constraint -> [Constraint] -> [Constraint]
reduceConstraints ctx cs = catMaybes $ (ctx ?~) <$> cs

-- | I couldn't come up with a good name for this. It's like sequence (and I was
-- using sequence before), but instead, it picks a constraint from the constraint set, then
-- first "reduces" ALL of the constraints in the remaining constraint sets. Here,
-- "reduce" means calling reduceConstraints with the chosen constraint to each of the
-- constraint sets. The reason we bother to do this is that it would just be insanely
-- inefficient if we didn't proactively filter the constraints in this way.
collect :: [[Constraint]] -> [[Constraint]]
collect (cs:css) = do
  c' <- cs
  let reduced = reduceConstraints c' <$> css -- reduce the remaining constraint sets
  cs' <- collect reduced
  return (c':cs')
collect [] = [[]]

-- | Given a list of constraints, unifies all of them into a single constraint, or
-- fails if any two disagree.
matchConstraints :: [Constraint] -> Maybe Constraint
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
  let scenarios = collect (cellConstraints board <$> boundary board)
  in  catMaybes (matchConstraints <$> scenarios)

-- | Given a board, returns a list of all the potential next neighbor-moves, along
-- with the odds that each move will not contain a mine.
movesWithOdds :: MSBoard board => board -> [((Int,Int), Ratio Int)]
movesWithOdds board = [ (ix, odds ix) | ix <- frontier board ]
  where scenarios = neighborScenarios board
        odds ix = (fromIntegral $ getSum $ foldMap (Sum . count) scenarios) /
                  (fromIntegral $ length scenarios)
          where count scenario = maybe 0 fromEnum $ M.lookup ix scenario
