{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Data.Monoid ((<>))
import Data.Ratio
import qualified Graphics.Vty as V
import System.Exit (exitFailure)
import System.IO

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import Brick.Widgets.Core
-- import Data.Text.Zipper (moveCursor)
import Data.Tuple (swap)

import Data.MSBoard.Classes
import Data.MSBoard.Expert
import Data.MSBoard.IO
import Data.MSBoard.SimpleBoard
import Data.MSBoard.State

data Name = BoardClick
  deriving (Eq, Ord, Show)

data St = St { _board   :: SimpleBoard
             , _focused :: Maybe Name
             , _status  :: String
             , _result  :: GameResult
             }

makeLenses ''St

drawStat :: Int -> String -> Widget Name
drawStat n label = str label <+> (withDefAttr statAttr $ hLimit 10 $ C.hCenter $ str $ show n)

drawBoard :: SimpleBoard -> Widget Name
drawBoard b = clickable BoardClick $ str (showBoard b)

drawHelp :: Widget Name
drawHelp = str $
  "How to play \n\
  \\n\
  \q                      quit\n\
  \n                      new game\n\
  \<left click>           breach cell\n\
  \Ctrl+<left click>      flag cell\n\
  \Meta+<left click>      get bomb odds for cell\
  \"

title :: Widget Name
title = str $
  "  M   M    IIIIIIIII  NN      N  EEEEEEEEE  SSSSSSSS  OOOOOOO  L         V       V EEEEEEEEE RRRRRRRR \n\
  \  MM MM        I      N NN    N  E         S         O       O L          V     V  E         R       R\n\
  \ M  M  M       I      N   N   N  EEEEE      SSSSSSS  O       O L           V   V   EEEEE     RRRRRRRR \n\
  \ M     M       I      N    NN N  E                 S O       O L            V V    E         R    RR  \n\
  \M       M  IIIIIIIII  N      NN  EEEEEEEEE SSSSSSSS   OOOOOOO  LLLLLLLLL     V     EEEEEEEEE R      RR\n"


-- | Convert mouse click in board to actual cell coordinate
coordToCell :: (Int, Int) -> (Int, Int)
coordToCell (x,y) = (y, x `div` 5)

drawUi :: St -> [Widget Name]
drawUi st =
  [ vBox
    [ padTop (T.Pad 5) $
      C.hCenter $
      title
    , hBox [ C.hCenter $
             padTop (T.Pad 2) $
             withBorderStyle B.unicode $
             C.vCenter $
             B.borderWithLabel (str "Hello!") $
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
        (h, w) = dims b

showRat :: Ratio Int -> String
showRat r = show (numerator r) ++ "/" ++ show (denominator r)

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'n') [])) = do
  b <- liftIO $ (randomBoard (20,20) 80 :: IO SimpleBoard)
  M.continue $ st & (board .~ b) & (result .~ C)
appEvent st (T.MouseDown BoardClick V.BLeft [V.MCtrl] coords) = do
  let cell = coordToCell (T.loc coords)
  let (res, b) = runState (toggleFlagSt cell) (st ^. board)
  M.continue $ st & (board .~ b)
appEvent st (T.MouseDown BoardClick V.BLeft [V.MMeta] coords) = case st ^. result of
  C -> do
    let cell = coordToCell (T.loc coords)
    let moves = movesWithOdds (st ^. board)
    case lookup cell moves of
      -- FIXME: In this case, we can give odds based on overall board state
      Nothing -> M.continue $ st & (status .~ " ")
      Just r  -> M.continue $ st & (status .~ showRat r ++ " bomb probability")
appEvent st (T.MouseDown BoardClick V.BLeft [] coords) = case st ^. result of
  C -> do
    let cell = coordToCell (T.loc coords)
    let (res, b') = runState (pushSt cell >> gameResult) (st ^. board)
    let b = case res of
          L -> execState pushAllSt b'
          _ -> b'
    let newStatus = case res of
          W -> "You won!"
          L -> "You lost! Ha-ha!"
          C -> st ^. status
    M.continue $ st & (board .~ b) & (result .~ res) & (status .~ newStatus)
  _ -> M.continue st
appEvent st _ = M.continue st

statAttr :: AttrName
statAttr = "hi"

msAttrMap :: St -> AttrMap
msAttrMap = const $ attrMap V.defAttr [(statAttr, V.white `on` V.blue)]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = msAttrMap
          , M.appChooseCursor = M.showFirstCursor
          }

-- main :: IO ()
-- main = do
--   hSetBuffering stdout NoBuffering
--   board <- randomBoard (5,5) 7 :: IO SimpleBoard
--   finalBoard <- execStateT play board
--   putStrLn "Thanks for playing! Bye!"


checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

main = do
  checkForMouseSupport
  board <- randomBoard (20,20) 80 :: IO SimpleBoard
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  st <- M.customMain buildVty Nothing app $
    St
    board
    Nothing
    "Welcome to MineSolver!"
    C
  return ()

test :: (Int, Int) -> IO ()
test ix = do
  board <- randomBoard (5,5) 4 :: IO SimpleBoard
  flip runStateT board $ do
    pushSt ix
    printBoard
    board <- get
    lift $ mapM_ print $ movesWithOdds board
  return ()
