module Data.MSBoard.Classes
  ( MSBoard
  , blankBoard
  , dims
  , cellInfo
  , boardRange
  , neighbors
  ) where

import Control.Monad.State
import Data.Ix
-- import System.Random

import Data.MSBoard.Types

class MSBoard board where

  -- | Given the size of the board and a list of bomb locations, construct a board
  blankBoard :: (Int, Int) -> [(Int, Int)] -> board

  -- | The (height, width) dimensions of the board
  dims :: State board (Int, Int)

  -- | Given a cell, return the number of nearby bombs
  cellInfo :: (Int, Int) -> State board (Maybe CellInfo)

-- | Return the range of coordinates within the board
boardRange :: MSBoard board => State board ((Int, Int), (Int, Int))
boardRange = do
  (h,w) <- dims
  return $ ((0,0), (h-1,w-1))

-- | Return a list of all the neighbors of a particular node in a board
neighbors :: MSBoard board => (Int, Int) -> State board [(Int, Int)]
neighbors (r,c) = do
  let candidates = [ (r + rOff, c + cOff) | rOff <- [-1,0,1]
                                          , cOff <- [-1,0,1]
                                          ]
  range <- boardRange
  return $ filter (inRange range) candidates

-- | Construct a random board from a random number generator, dimensions, and number
-- of bombs
-- randomBoard :: (Int, Int) -> Int -> IO board
-- randomBoard (h,w) numBombs = do
--   gen <- getStdGen
  
