module Data.MSBoard.SimpleBoard where

import           Control.Monad.State
import           Data.Ix
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.MSBoard.Classes
import Data.MSBoard.Types

data SimpleBoard = SimpleBoard (Int, Int) (Map (Int, Int) CellInfo)

emptyCell :: CellInfo
emptyCell = CellInfo False False False

emptyBoard :: (Int, Int) -> SimpleBoard
emptyBoard (h,w) = SimpleBoard (h,w) (Map.fromList (toEmptyCell <$> range ((0,0), (h-1,w-1))))
  where toEmptyCell x = (x, emptyCell)

instance MSBoard SimpleBoard where
  blankBoard dims bombs = emptyBoard dims

  dims = do
    SimpleBoard dims _ <- get
    return dims

  cellInfo cell = do
    SimpleBoard _ cells <- get
    return $ Map.lookup cell cells

numBombs :: SimpleBoard -> Int
numBombs = const 0
