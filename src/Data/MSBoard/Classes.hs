module Data.MSBoard.Classes
  ( MSBoard (..)
  , bombPushed
  , boardRange
  , neighbors
  , numNeighboringBombs
  , showBoard
  ) where

import Data.Ix
import Data.Monoid
-- import System.Random

import Data.MSBoard.Types

class MSBoard board where

  -- | Given the size of the board and a list of bomb locations, construct a board
  blankBoard :: (Int, Int) -> [(Int, Int)] -> board

  -- | The (height, width) dimensions of the board
  dims :: board -> (Int, Int)

  -- | The number of bombs in the board
  numBombs :: board -> Int

  -- | Given a cell, return all info about that cell. Return Nothing if index is out
  -- of bounds.
  cellInfo :: board -> (Int, Int) -> Maybe CellInfo

  -- | Push a cell.
  push :: (Int, Int) -> board -> board

  toggleFlag :: (Int, Int) -> board -> board

-- | Return whether any bombs have been pushed on the board
bombPushed :: MSBoard board => board -> Bool
bombPushed board = flip any (range $ boardRange board) $ \ix ->
  case cellInfo board ix of
    Nothing -> False
    Just i  -> cellHasBomb i && cellIsPushed i

-- | Return the range of coordinates within the board
boardRange :: MSBoard board => board -> ((Int, Int), (Int, Int))
boardRange board = ((0,0), (h-1,w-1))
  where (h,w) = dims board

-- | Return a list of all the neighbors of a particular node in a board
neighbors :: MSBoard board => board -> (Int, Int) -> [(Int, Int)]
neighbors board (r,c) = filter (inRange (boardRange board)) candidates
  where candidates = [ (r + rOff, c + cOff) | rOff <- [-1,0,1]
                                            , cOff <- [-1,0,1]
                                            ]

numNeighboringBombs :: MSBoard board => board -> (Int, Int) -> Int
numNeighboringBombs board idx = getSum $ foldMap bombCount (neighbors board idx)
  where bombCount idx' = case cellInfo board idx' of
          Nothing -> error $ "Cell out of bounds: " ++ show idx
          Just i  -> Sum $ fromEnum (cellHasBomb i)

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
        showCell (r, c) = case cellInfo board (r,c) of
          Nothing -> error $ "Cell out of bounds: " ++ show (r,c)
          Just i  -> " " ++ showInfo i ++ end
          where end = if (c+1) `mod` width board == 0
                      then "\n"
                      else ""
                showInfo i = case cellIsPushed i of
                  False -> case cellIsFlagged i of
                    False -> " [ ]"
                    True  -> " [!]"
                  True  -> case cellHasBomb i of
                    True -> " [X]"
                    False -> "  " ++ show (numNeighboringBombs board (r,c)) ++ " "

