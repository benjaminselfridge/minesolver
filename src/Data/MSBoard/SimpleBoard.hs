-- | This module exports the SimpleBoard data type, with an MSBoard instance
-- declaration for said data type. Internally, a SimpleBoard is just a map from (Int,
-- Int) cell indices to information about the cell.

{-# LANGUAGE TemplateHaskell #-}

module Data.MSBoard.SimpleBoard
  ( SimpleBoard ) where

import           Data.Ix
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Lens.Micro ( (&), (^.), (%~))
import           Lens.Micro.TH (makeLenses)

import Data.MSBoard.Classes

data CellInfo = CellInfo { hasBomb :: Bool
                         , isPushed :: Bool
                         , isFlagged :: Bool
                         }

data SimpleBoard = SimpleBoard { _sbDims :: (Int, Int)
                               , _sbCells :: (Map (Int, Int) CellInfo)
                               }

makeLenses ''SimpleBoard

emptyCell :: CellInfo
emptyCell = CellInfo False False False

emptyBoard :: (Int, Int) -> SimpleBoard
emptyBoard (h,w) = SimpleBoard (h,w) (M.fromList (toEmptyCell <$> range ((0,0), (h-1,w-1))))
  where toEmptyCell x = (x, emptyCell)

pushCell :: CellInfo -> CellInfo
pushCell i = i { isPushed = True }

addBombCell :: CellInfo -> CellInfo
addBombCell i = i { hasBomb = True }

toggleFlagCell :: CellInfo -> CellInfo
toggleFlagCell i = if isFlagged i
                   then i { isFlagged = False }
                   else i { isFlagged = True }

instance MSBoard SimpleBoard where
  blankBoard sz bombs = foldr (\idx -> (sbCells %~ M.adjust addBombCell idx)) (emptyBoard sz) bombs

  dims board = board ^. sbDims

  cellHasBomb board cell = maybe False hasBomb $ M.lookup cell (board ^. sbCells)

  cellIsPushed board cell = maybe False isPushed $ M.lookup cell (board ^. sbCells)

  cellIsFlagged board cell = maybe False isFlagged $ M.lookup cell (board ^. sbCells)

  push idx board = board & (sbCells %~ M.adjust pushCell idx)

  toggleFlag idx board = board & (sbCells %~ M.adjust toggleFlagCell idx)
