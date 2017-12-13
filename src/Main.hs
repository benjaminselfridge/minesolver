{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
-- import Lens.Micro ((^.), (&), (.~), (%~))
-- import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

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

import Data.MSBoard.SimpleBoard

data Name = Name
  deriving (Eq, Ord)

drawStat :: Int -> String -> Widget Name
drawStat n label = str label <+> (withDefAttr statAttr $ hLimit 10 $ C.hCenter $ str $ show n)

drawUi :: SimpleBoard -> [Widget Name]
drawUi b@(SimpleBoard (h,w) _) = [ C.hCenter $
                                   padTop (T.Pad 4) $
                                   hBox [ padRight (T.Pad 10) $ drawStat h "H:"
                                        , padRight (T.Pad 10) $ drawStat w "W:"
                                        , drawStat (numBombs b) "Bombs:"
                                        ]
                                 ]

appEvent :: SimpleBoard -> T.BrickEvent Name e -> T.EventM Name (T.Next SimpleBoard)
appEvent b (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt b
appEvent b _ = M.continue b

statAttr :: AttrName
statAttr = "hi"

msAttrMap :: SimpleBoard -> AttrMap
msAttrMap = const $ attrMap V.defAttr [(statAttr, V.white `on` V.blue)]

app :: M.App SimpleBoard e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = msAttrMap
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = void $ M.defaultMain app (emptyBoard (10,10))
