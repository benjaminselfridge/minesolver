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
import qualified Graphics.Vty as V
import System.IO

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
-- import Data.Text.Zipper (moveCursor)
import Data.Tuple (swap)

import Data.MSBoard.Classes
import Data.MSBoard.Expert
import Data.MSBoard.IO
import Data.MSBoard.SimpleBoard
import Data.MSBoard.State

data Name = WidthEdit
          | HeightEdit
          | BombsEdit
  deriving (Eq, Ord, Show)

data St = St { _board   :: SimpleBoard
             , _focused :: Maybe Name
             , _width   :: E.Editor String Name
             , _height  :: E.Editor String Name
             , _bombs   :: E.Editor String Name

             }

makeLenses ''St

drawStat :: Int -> String -> Widget Name
drawStat n label = str label <+> (withDefAttr statAttr $ hLimit 10 $ C.hCenter $ str $ show n)

drawUi :: St -> [Widget Name]
drawUi st = [ C.hCenter $
              padTop (T.Pad 4) $
              hBox [ padRight (T.Pad 10) $ drawStat h "H:"
                   , padRight (T.Pad 10) $ drawStat w "W:"
                   , drawStat (numBombs b) "Bombs:"
                   ]
            ]
  where b = st^.board
        (h, w) = dims b

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent b (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt b
appEvent b _ = M.continue b

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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  board <- randomBoard (10,10) 20 :: IO SimpleBoard
  finalBoard <- execStateT play board
  putStrLn "Thanks for playing! Bye!"



-- main = void $ M.defaultMain app (St
--                                   (emptyBoard (10,10))
--                                   Nothing
--                                   (E.editor HeightEdit (Just 1) "10")
--                                   (E.editor WidthEdit  (Just 1) "10")
--                                   (E.editor BombsEdit  (Just 1) "10"))

test :: (Int, Int) -> IO ()
test ix = do
  board <- randomBoard (5,5) 4 :: IO SimpleBoard
  flip runStateT board $ do
    pushSt ix
    printBoard
    board <- get
    lift $ mapM_ print $ movesWithOdds board
  return ()
