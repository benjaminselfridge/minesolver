-- | This module provides the brick-based gui frontend for a simple minesweeper game
-- that gives helpful feedback to the user on bomb odds. It is the main module of the
-- minesolver app.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)
import Data.Ratio
import qualified Graphics.Vty as V
import System.Exit (exitFailure)

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Types (Widget)
import qualified Brick.Main as M
-- import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import Brick.Widgets.Core

import Data.MSBoard.Classes
import Data.MSBoard.Expert
import Data.MSBoard.IO
import Data.MSBoard.SimpleBoard
import Data.MSBoard.State

-- | Widgets that need to be able to handle events
data Name = BoardClick
  deriving (Eq, Ord, Show)

-- | Application state
data St = St { _board       :: SimpleBoard      -- ^ the board state
             , _focused     :: Maybe Name       -- ^ we don't need this yet
             , _status      :: String           -- ^ changeable status string
             , _result      :: GameResult       -- ^ game result
             , _clickedCell :: Maybe (Int, Int) -- ^ current clicked cell
             }

makeLenses ''St

drawBoard :: SimpleBoard -> Widget Name
drawBoard b = clickable BoardClick $ str (showBoard b)

drawHelp :: Widget Name
drawHelp = str $
  "q                      quit\n\
  \n                      new game\n\
  \<left click>           breach cell\n\
  \<right click>          flag cell\n\
  \Meta+<left click>      get bomb odds for cell\
  \"

title :: Widget Name
title = str $
  " _______ _____ __   _ _______ _______  _____         _    _ _______  ______\n\
  \ |  |  |   |   | \\  | |______ |______ |     | |       \\  /  |______ |_____/\n\
  \ |  |  | __|__ |  \\_| |______ ______| |_____| |_____   \\/   |______ |    \\_\n"

-- | Convert mouse click in board to actual cell coordinate
coordToCell :: (Int, Int) -> Maybe (Int, Int)
coordToCell (x,y) = case x `mod` 4 >= 1 && x `mod` 4 <= 3 of
  True  -> Just (y, x `div` 4)
  False -> Nothing

-- | Draw the user interface
drawUi :: St -> [Widget Name]
drawUi st =
  [ vBox
    [ padTop (T.Pad 5) $
      C.hCenter $
      title
    , hBox [ C.hCenter $
             withBorderStyle B.unicode $
             C.vCenter $
             B.border $
             padRight (T.Pad 1) $
             drawBoard b
           ]
    , C.hCenter $
      str stat
    , C.hCenter $
      padTop (T.Pad 2) $
      padBottom (T.Pad 4) $
      drawHelp
    ]
  ]
  where b = st^.board
        stat = st^.status

-- | convenience function for appEvent
maybeContinue :: St -> Maybe a -> (a -> T.EventM Name (T.Next St)) -> T.EventM Name (T.Next St)
maybeContinue st Nothing  _ = M.continue st
maybeContinue _  (Just a) k = k a

-- | The event handler for minesolver
appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  b <- liftIO $ (randomBoard (20,20) 60 :: IO SimpleBoard)
  M.continue $ st
    & (board .~ b)
    & (result .~ Continue)
    & (status .~ "Good luck!")
appEvent st (T.MouseDown BoardClick V.BLeft [] coords) = case st ^. result of
  Continue -> maybeContinue st (coordToCell $ T.loc coords) $ \cell -> do
    let (res, b') = runState (pushSt cell >> gameResult) (st ^. board)
    let b = case res of
          Loss -> execState revealBoardSt b'
          _ -> b'
    let newStatus = case res of
          Win -> "You won!"
          Loss -> "You lost! Ha-ha!"
          Continue -> st ^. status
    M.continue $ st
      & (board .~ b)
      & (result .~ res)
      & (status .~ newStatus)
      & (clickedCell .~ Just cell)
  _ -> M.continue st
appEvent st (T.MouseDown BoardClick V.BLeft [V.MMeta] coords) = case st ^. result of
  Continue ->
    maybeContinue st (coordToCell $ T.loc coords) $ \cell -> do
    let moves = movesWithOdds (st ^. board)
    case lookup cell moves of
      -- FIXME: In this case, we can give odds based on overall board state
      Nothing -> M.continue $ st & (status .~ "Not sure...")
      Just r  -> M.continue $ st
        & (status .~ sr ++ " bomb probability")
        where sr = show (numerator r) ++ "/" ++ show (denominator r)
  _ -> M.continue st
appEvent st (T.MouseDown BoardClick V.BRight [] coords) =
  maybeContinue st (coordToCell $ T.loc coords) $ \cell -> do
  case maybe False (==cell) (st ^. clickedCell) of
    False -> do
      let (_, b) = runState (toggleFlagSt cell) (st ^. board)
      M.continue $ st & (board .~ b) & (clickedCell .~ Just cell)
    True -> M.continue st
appEvent st (T.MouseDown _ _ _ _) = M.continue $ st & (clickedCell .~ Nothing)
appEvent st (T.MouseUp _ _ _) = M.continue $ st & (clickedCell .~ Nothing)
appEvent st _ = M.continue st

-- | For now, an empty attribute map
msAttrMap :: St -> AttrMap
msAttrMap = const $ attrMap V.defAttr []

-- | data structure that represents the entire app
app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = msAttrMap
          , M.appChooseCursor = M.showFirstCursor
          }

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

main :: IO ()
main = do
  checkForMouseSupport
  b <- randomBoard (20,20) 60 :: IO SimpleBoard
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  _st <- M.customMain buildVty Nothing app $
    St { _board = b
       , _focused = Nothing
       , _status = "Welcome to MineSolver!"
       , _result = Continue
       , _clickedCell = Nothing
       }
  return ()

-- main :: IO ()
-- main = do
--   hSetBuffering stdout NoBuffering
--   board <- randomBoard (5,5) 7 :: IO SimpleBoard
--   finalBoard <- execStateT play board
--   putStrLn "Thanks for playing! Bye!"

-- statAttr :: AttrName
-- statAttr = "hi"

-- msAttrMap :: St -> AttrMap
-- msAttrMap = const $ attrMap V.defAttr [(statAttr, V.white `on` V.blue)]
