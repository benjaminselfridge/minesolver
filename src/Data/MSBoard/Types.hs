module Data.MSBoard.Types
  ( CellInfo(..) ) where

data CellInfo = CellInfo { cellHasBomb :: Bool
                         , cellIsPushed :: Bool
                         , cellIsFlagged :: Bool
                         }
