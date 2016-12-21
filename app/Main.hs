module Main where

import Lib
import Play

import FreeGame
import Data.Maybe
import Control.Monad
import Data.List
import Debug.Trace as D
import Data.Array

data Scene = Opening | Play | GameOver

update :: [Bitmap] -> Font -> Scene -> Game ()
update picts font Opening = do
  l <- mouseDownL

  let newScene = if l then Play else Opening

  translate (V2 80 200) . color black $ text font 40 "くろーばーならべ"
  translate (V2 80 300) . color black $ text font 40 "click to start"

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update picts font newScene

update picts font Play = do
  let fieldLen = 8 :: Int
      -- plots = do
      --   let lenDouble = fromIntegral fieldLen :: Double
      --       plotList = [25,75..25+50*(lenDouble - 1)]
      --   y <- plotList
      --   x <- plotList
      --   return (x,y) :: [Plot]
      indexTuple = do
        let indexRange = [0..fieldLen-1]
        x <- indexRange
        y <- indexRange
        return (x,y)
      emptyField =  array ((0,0), (fieldLen-1, fieldLen-1)) $ zip indexTuple [Empty, Empty ..] :: Array (Int,Int) Clover


  -- 無限ループさせる（終了条件は、どちらかが勝った時）
  (resultField, turn) <- playing emptyField picts Green font fieldLen
  -- 無限ループさせる（終了条件は、クリックした時）
  gameOver resultField picts font turn
  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update picts font Play

-- update picts font GameOver = do
--   drawGrid -- 盤の描画
--   drawClovers field plots picts
--   translate (V2 80 200) . color black $ text font 40 (show turn ++ "の勝ちです。")
--   translate (V2 80 300) . color black $ text font 40 "もういちど、あそぶときは\n画面をクリックしてくださいっ。"
--   l <- mouseDownL
--   let fieldLen = 8 :: Int
--       emptyField = replicate fieldLen $ replicate fieldLen Empty
--   tick
--   if l then update emptyField plots picts Green font Play
--     else update field plots picts turn font GameOver

main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
  clearColor white
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"
  font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
  -- let fieldLen = 8 :: Int

      -- plots = do
      --   let lenDouble = fromIntegral fieldLen :: Double
      --       plotList = [25,75..25+50*(lenDouble - 1)]
      --   y <- plotList
      --   x <- plotList
      --   return (x,y) :: [Plot]
      -- emptyField = replicate fieldLen $ replicate fieldLen Empty
      -- emptyField = array (head plots, last plots) [zip plots [Empty, Empty ..]]

  update [gCloverPict, rCloverPict] font Opening
