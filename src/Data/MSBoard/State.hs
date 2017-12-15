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

getRange :: (Monad m, MSBoard board) => StateT board m ((Int, Int), (Int, Int))
getRange = boardRange <$> get

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

flagSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
flagSt ix = do
  state $ ((),) . toggleFlag ix

data GameResult = W | L | C

gameResult :: (Monad m, MSBoard board) => StateT board m GameResult
gameResult = do
  board <- get
  case anyBombPushed board of
    True  -> return L
    False -> case allSafePushed board of
      True  -> return W
      False -> return C

pushAllSt :: (Monad m, MSBoard board) => StateT board m ()
pushAllSt = do
  board <- get
  forM_ (range $ boardRange board) $ \ix -> do
    pushSt ix
