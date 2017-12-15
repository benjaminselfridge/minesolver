{-# LANGUAGE TupleSections #-}

module Data.MSBoard.State
  ( pushSt
  , flagSt
  , pushAllSt
  , gameResult
  , GameResult(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Ix

import Data.MSBoard.Classes
import Data.MSBoard.Types

getRange :: (Monad m, MSBoard board) => StateT board m ((Int, Int), (Int, Int))
getRange = boardRange <$> get

pushSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
pushSt ix = do
  board <- get
  case flagged board ix of
    True -> return ()
    False -> do
      state $ ((),) . push ix
      board <- get
      case numNeighboringBombs board ix of
        0 -> forM_ (filter (notPushed board) (neighbors board ix)) pushSt
        _ -> return ()
  where notPushed board ix = case cellInfo board ix of
          Nothing -> True
          Just i  -> not (cellIsPushed i)
        flagged board ix = case cellInfo board ix of
          Nothing -> False
          Just i  -> cellIsFlagged i

flagSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
flagSt ix = do
  state $ ((),) . toggleFlag ix

data GameResult = W | L | C

gameResult :: (Monad m, MSBoard board) => StateT board m GameResult
gameResult = do
  board <- get
  case bombPushed board of
    True  -> return L
    False -> return C

pushAllSt :: (Monad m, MSBoard board) => StateT board m ()
pushAllSt = do
  board <- get
  forM_ (range $ boardRange board) $ \ix -> do
    pushSt ix
