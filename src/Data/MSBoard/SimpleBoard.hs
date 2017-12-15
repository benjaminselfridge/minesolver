{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.MSBoard.SimpleBoard
  ( SimpleBoard ) where

import           Control.Monad.State
import           Data.Ix
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Lens.Micro ( (&), (^.), (.~), (%~), Lens', lens)
import           Lens.Micro.TH (makeLenses)

import Data.MSBoard.Classes
import Data.MSBoard.Types

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
pushCell i = i { cellIsPushed = True }

addBomb :: CellInfo -> CellInfo
addBomb i = i { cellHasBomb = True }

addFlag :: CellInfo -> CellInfo
addFlag i = i { cellHasFlag = True }

instance MSBoard SimpleBoard where
  blankBoard dims bombs = foldr (\idx -> (sbCells %~ M.adjust addBomb idx)) (emptyBoard dims) bombs

  dims board = board ^. sbDims

  cellInfo board cell = M.lookup cell (board ^. sbCells)

  -- FIXME
  numBombs = const 0

  push idx board = board & (sbCells %~ M.adjust pushCell idx)

  flag idx board = board & (sbCells %~ M.adjust addFlag idx)
