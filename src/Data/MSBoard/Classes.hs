module Data.MSBoard.Classes
  ( MSBoard (..)
  , numBombs
  , anyBombPushed
  , allSafePushed
  , boardRange
  , neighbors
  , unpushedNeighbors
  , numUnpushedNeighbors
  , allUnpushedNeighbors
  , allPushedNeighbors
  , numNeighboringBombs
  , showBoard
  ) where

import Data.Ix
import Data.List ((\\))
import Data.Monoid
-- import System.Random

class MSBoard board where

  -- | Given the size of the board and a list of bomb locations, construct a board
  blankBoard :: (Int, Int) -> [(Int, Int)] -> board

  -- | The (height, width) dimensions of the board
  dims :: board -> (Int, Int)

  -- | Test if a particular cell has a bomb
  cellHasBomb :: board -> (Int, Int) -> Bool

  -- | Test if a particular cell is pushed
  cellIsPushed :: board -> (Int, Int) -> Bool

  -- | Test if a particular cell is flagged
  cellIsFlagged :: board -> (Int, Int) -> Bool

  -- | Push a cell.
  push :: (Int, Int) -> board -> board

  -- | Toggle whether a cell is flagged.
  toggleFlag :: (Int, Int) -> board -> board

-- | Return the number of bombs on the board
numBombs :: MSBoard board => board -> Int
numBombs board = length $ filter (cellHasBomb board) $ range $ boardRange board

-- | Return whether any bombs have been pushed on the board
anyBombPushed :: MSBoard board => board -> Bool
anyBombPushed board = flip any (range $ boardRange board) pushedBomb
  where pushedBomb ix = cellHasBomb board ix && cellIsPushed board ix

allSafePushed :: MSBoard board => board -> Bool
allSafePushed board = not $ flip any (range $ boardRange board) safeUnpushed
  where safeUnpushed ix = not (cellHasBomb board ix) && not (cellIsPushed board ix)

-- | Return the range of coordinates within the board
boardRange :: MSBoard board => board -> ((Int, Int), (Int, Int))
boardRange board = ((0,0), (h-1,w-1))
  where (h,w) = dims board

-- FIXME: Clean the below functions up, they are confusing as hell

-- | Return a list of all the neighbors of a particular node in a board
neighbors :: MSBoard board => board -> (Int, Int) -> [(Int, Int)]
neighbors board (r,c) = filter (inRange (boardRange board)) candidates
  where candidates = [ (r + rOff, c + cOff) | rOff <- [-1,0,1]
                                            , cOff <- [-1,0,1]
                                            ] \\ [(r,c)]

unpushedNeighbors :: MSBoard board => board -> (Int, Int) -> [(Int, Int)]
unpushedNeighbors board ix = filter (not . cellIsPushed board) (neighbors board ix)

numUnpushedNeighbors :: MSBoard board => board -> (Int, Int) -> Int
numUnpushedNeighbors board ix = getSum $ foldMap (Sum . fromEnum . not . cellIsPushed board) (neighbors board ix)

allUnpushedNeighbors :: MSBoard board => board -> [(Int, Int)]
allUnpushedNeighbors board = filter unpushedNeighbor (range $ boardRange board)
  where unpushedNeighbor ix = not (cellIsPushed board ix) && hasPushedNeighbors ix
        hasPushedNeighbors ix = any (cellIsPushed board) (neighbors board ix)

allPushedNeighbors :: MSBoard board => board -> [(Int, Int)]
allPushedNeighbors board = filter pushedNeighbor (range $ boardRange board)
  where pushedNeighbor ix = cellIsPushed board ix && numNeighboringBombs board ix > 0

numNeighboringBombs :: MSBoard board => board -> (Int, Int) -> Int
numNeighboringBombs board ix = getSum $ foldMap (Sum . fromEnum . cellHasBomb board) (neighbors board ix)

height :: MSBoard board => board -> Int
height = fst . dims

width :: MSBoard board => board -> Int
width = snd . dims

replicateM :: Monoid m => Int -> m -> m
replicateM k m | k > 0 = m <> replicateM (k-1) m
               | otherwise = mempty

showBoard :: MSBoard board => board -> String
showBoard board = -- replicateM (width board) "_____" ++ "\n" ++
                  rows
                  -- concat (zipWith (++) (show <$> [1..height board]) rows)
  where rows = foldMap showCell (range $ boardRange board)
        showL i = " " ++ (replicate (5 - length (show i)) ' ') ++ show i ++ " |"
        showCell (r,c) = " " ++ showInfo (r,c) ++ end
          where end = if (c+1) `mod` width board == 0
                      then "\n"
                      else ""
                showInfo ix = case cellIsPushed board ix of
                  False -> case cellIsFlagged board ix of
                    False -> " [ ]"
                    True  -> " [!]"
                  True  -> case cellHasBomb board ix of
                    True -> " [X]"
                    False -> "  " ++ show (numNeighboringBombs board ix) ++ " "

