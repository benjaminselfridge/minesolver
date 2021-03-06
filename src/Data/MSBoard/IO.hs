-- | This module exports functions specifically for the essentially defunct command
-- line interface.

module Data.MSBoard.IO
  ( shuffle
  , randomBoard
  , printBoard
  , play
  ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Array.ST
import Data.STRef
import System.Random
import Text.Read (readMaybe)

import Data.MSBoard.Classes
import Data.MSBoard.Expert
import Data.MSBoard.State

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle :: RandomGen g => [a] -> g -> ([a],g)
shuffle xs gen = runST $ do
  g <- newSTRef gen
  let randomRST lohi = do
        (a,s') <- liftM (randomR lohi) (readSTRef g)
        writeSTRef g s'
        return a
  ar <- newArray' n xs
  xs' <- forM [1..n] $ \i -> do
    j <- randomRST (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  gen' <- readSTRef g
  return (xs',gen')
  where
    n = length xs
    newArray' :: Int -> [a] -> ST s (STArray s Int a)
    newArray' k ys =  newListArray (1,k) ys

-- | Construct a random board from a random number generator, dimensions, and number
-- of bombs
randomBoard :: MSBoard board => (Int, Int) -> Int -> IO board
randomBoard (h,w) nBombs = do
  gen <- newStdGen
  let (bombs, _) = shuffle (range ((0,0),(h-1,w-1))) gen
  return $ blankBoard (h,w) (take nBombs bombs)

printBoard :: MSBoard board => StateT board IO ()
printBoard = do
  board <- get
  lift $ putStr (showBoard board)

helpString :: String
helpString =
  "b (<r>, <c>)      -- breach at position (<r>, <c>)\n" ++
  "                     (index starting at 0)\n" ++
  "f (<r>, <c>)      -- place flag at position (<r>, <c>)\n" ++
  "e                 -- get advice on next move\n" ++
  "h                 -- print this help message\n" ++
  "q                 -- quit"

play :: MSBoard board => StateT board IO ()
play = do
  printBoard
  lift $ putStr "Enter command (h for help): "
  (com:args) <- lift $ words <$> getLine
  case com of
    "b" -> case readMaybe (concat args) of
      Nothing -> play
      Just ix -> do pushSt ix
                    res <- gameResult
                    case res of
                      Win -> lift $ putStrLn "Nice, you won!"
                      Loss -> do lift $ putStrLn "You lost! Ha-ha!"
                                 printBoard
                      Continue -> play
    "f" -> case readMaybe (concat args) of
      Nothing -> play
      Just ix -> do toggleFlagSt ix
                    play
    "e" -> do board <- get
              -- TODO: print this out more elegantly. Perhaps, find the one with the
              -- lowest odds, then compare that to the overall odds based on ratio of
              -- remaining cells to remaining bombs where "remaining" means "so far
              -- undetected, but we know are there because of the bomb count."
              let moves = movesWithOdds board
              let bestMove = case moves of
                    [] -> Nothing
                    (m:ms) -> Just $ foldl (minBy snd) m ms
                      where minBy f a b = case f a < f b of
                                            True  -> a
                                            False -> b
              case bestMove of
                Just (ix, r) -> do lift $ putStrLn $ show ix ++ ", bomb probability of " ++ show r
                                   play
                Nothing -> return ()
    "h" -> do lift $ putStrLn helpString
              play
    "q" -> return ()
    _ -> case readMaybe (concat (com:args)) of
      Nothing -> do lift $ putStrLn $ "Unrecognized command " ++ com
                    play
      Just ix -> do pushSt ix
                    res <- gameResult
                    case res of
                      Win -> do printBoard
                                lift $ putStrLn "Nice, you won!"
                      Loss -> do revealBoardSt
                                 printBoard
                                 lift $ putStrLn "You lost! Ha-ha!"
                      Continue -> play
