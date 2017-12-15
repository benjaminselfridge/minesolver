{-# LANGUAGE TupleSections #-}

module Data.MSBoard.State
  ( pushSt
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
getRange = do
  board <- get
  return $ boardRange board

pushSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m ()
pushSt ix = do
  state $ ((),) . push ix
  board <- get
  case numNeighboringBombs board ix of
    0 -> forM_ (filter (notPushed board) (neighbors board ix)) pushSt
    _ -> return ()
    where notPushed board ix = case cellInfo board ix of
            Nothing -> True
            Just i  -> not (cellIsPushed i)

-- pushSt :: (Monad m, MSBoard board) => (Int, Int) -> StateT board m Bool
-- pushSt idx = do
--   pushSt' idx
--   board <- get
--   case cellInfo board idx of
--     Just i ->
--       case cellHasBomb i of
--         True  -> return True
--         False -> return False
--     Nothing -> return False

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
