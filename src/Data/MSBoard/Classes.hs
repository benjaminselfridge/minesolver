-- | This module exports the MSBoard typeclass, which provides the basic interface
-- with a minesweeper board. It also provides some helpful functions for querying a
-- MSBoard.

module Data.MSBoard.Classes
  ( MSBoard(..)
  -- * Board functions
  , height
  , width
  , numBombs
  , anyBombPushed
  , allSafePushed
  , boardRange
  , frontier
  , boundary
  , showBoard
  -- * Cell functions
  , neighbors
  , unpushedNeighbors
  , numUnpushedNeighbors
  , numNeighboringBombs
  ) where

import Data.Ix
import Data.List ((\\))
import Data.Monoid

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

--------------------------------------------------------------------------------
-- Functions of the entire board

-- | height of board
height :: MSBoard board => board -> Int
height = fst . dims

-- | width of board
width :: MSBoard board => board -> Int
width = snd . dims

-- | Return the number of bombs on the board
numBombs :: MSBoard board => board -> Int
numBombs board = length $ filter (cellHasBomb board) $ range $ boardRange board

-- | Return the range of coordinates within the board
boardRange :: MSBoard board => board -> ((Int, Int), (Int, Int))
boardRange board = ((0,0), (h-1,w-1))
  where (h,w) = dims board

-- | Return a list of all the cells that have not yet been pushed, but border on a
-- cell that has.
frontier :: MSBoard board => board -> [(Int, Int)]
frontier board = filter unpushedNeighbor (range $ boardRange board)
  where unpushedNeighbor ix = not (cellIsPushed board ix) && hasPushedNeighbors ix
        hasPushedNeighbors ix = any (cellIsPushed board) (neighbors board ix)

-- | Return a list of all the cells that have been pushed, and border on a cell that
-- has not.
boundary :: MSBoard board => board -> [(Int, Int)]
boundary board = filter pushedNeighbor (range $ boardRange board)
  where pushedNeighbor ix = cellIsPushed board ix && numNeighboringBombs board ix > 0

-- | Return whether any bombs have been pushed on the board (losing condition)
anyBombPushed :: MSBoard board => board -> Bool
anyBombPushed board = flip any (range $ boardRange board) pushedBomb
  where pushedBomb ix = cellHasBomb board ix && cellIsPushed board ix

-- | Return whether ALL the safe cells have been pushed (winning condition)
allSafePushed :: MSBoard board => board -> Bool
allSafePushed board = not $ flip any (range $ boardRange board) safeUnpushed
  where safeUnpushed ix = not (cellHasBomb board ix) && not (cellIsPushed board ix)

-- | If a monoid gives a product operation, this is the corresponding exponent
-- function; given m in the monoid, this function gives m^k.
replicateM :: Monoid m => Int -> m -> m
replicateM k m | k > 0 = m <> replicateM (k-1) m
               | otherwise = mempty

-- | Render the board as a String.
showBoard :: MSBoard board => board -> String
showBoard board = foldMap showCell (range $ boardRange board)
  where showCell (r,c) = " " ++ showInfo (r,c) ++ end
          where end = if (c+1) `mod` width board == 0
                      then "\n"
                      else ""
                showInfo ix = case cellIsPushed board ix of
                  False -> case cellIsFlagged board ix of
                    False -> " . "
                    True  -> "!!!"
                  True  -> case cellHasBomb board ix of
                    True -> "XXX"
                    False -> case numNeighboringBombs board ix of
                      0 -> "   "
                      nBombs -> " " ++ show nBombs ++ " "
        showL i = " " ++ (replicate (4 - length (show i)) ' ') ++ show i ++ " |"

--------------------------------------------------------------------------------
-- Functions of individual cells

-- | Essentially length . filter, but more efficient.
countFilter :: (a -> Bool) -> [a] -> Int
countFilter f as = getSum $ foldMap (Sum . fromEnum . f) as

-- | Return a list of all the neighbors of a particular cell in a board
neighbors :: MSBoard board => board -> (Int, Int) -> [(Int, Int)]
neighbors board (r,c) = filter (inRange (boardRange board)) candidates
  where candidates = [ (r + rOff, c + cOff) | rOff <- [-1,0,1]
                                            , cOff <- [-1,0,1]
                                            ] \\ [(r,c)]

-- | Return a list of all the unpushed neighbors of a particular cell in a board
unpushedNeighbors :: MSBoard board => board -> (Int, Int) -> [(Int, Int)]
unpushedNeighbors board ix = filter (not . cellIsPushed board) (neighbors board ix)

-- | Return the number of unpushed neighbors of a particular cell in a board
numUnpushedNeighbors :: MSBoard board => board -> (Int, Int) -> Int
numUnpushedNeighbors board ix = countFilter (not . cellIsPushed board) (neighbors board ix)

-- | Return the number of neighboring bombs of a particular cell in a board
numNeighboringBombs :: MSBoard board => board -> (Int, Int) -> Int
numNeighboringBombs board ix = countFilter (cellHasBomb board) (neighbors board ix)

