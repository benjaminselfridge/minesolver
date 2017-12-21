-- | This module exports a number of state transitions for an MSBoard, built on top
-- of the more low-level functions provided in Data.MSBoard.Classes.

{-# LANGUAGE TupleSections #-}

module Data.MSBoard.State
  ( pushSt
  , toggleFlagSt
  , revealBoardSt
  , gameResult
  , GameResult(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Ix

import Data.MSBoard.Classes

-- | Push a cell on the board, and recursively push all the neighbors if the cell
-- does not have any neighboring bombs.
pushSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
pushSt ix = do
  board <- get
  case cellIsFlagged board ix of
    True -> return ()
    False -> do
      state $ ((),) . push ix
      board <- get
      case numNeighboringBombs board ix of
        0 -> forM_ (filter (not . cellIsPushed board) (neighbors board ix)) pushSt
        _ -> return ()

-- | Toggle the flag on a particular cell. This is a bare-bones wrapper around the
-- toggleFlag function from Data.MSBoard.Classes
toggleFlagSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
toggleFlagSt ix = do
  state $ ((),) . toggleFlag ix

-- | Game result
-- Win, Loss, or Continue (game still in progress)
data GameResult = W | L | C

-- | Get the game result from any particular board state
gameResult :: (Monad m, MSBoard board) => StateT board m GameResult
gameResult = do
  board <- get
  case anyBombPushed board of
    True  -> return L
    False -> case allSafePushed board of
      True  -> return W
      False -> return C

-- | Reveal all the cells in the board (after the game is won or lost)
revealBoardSt :: (Monad m, MSBoard board) => StateT board m ()
revealBoardSt = do
  board <- get
  forM_ (range $ boardRange board) $ \ix -> do
    pushSt ix
