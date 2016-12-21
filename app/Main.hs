module Main where

import Lib
import Play

import FreeGame
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
      indexTuple = do
        let indexRange = [0..fieldLen-1]
        x <- indexRange
        y <- indexRange
        return (x,y)
      emptyField =  array ((0,0), (fieldLen-1, fieldLen-1)) $ zip indexTuple [Empty, Empty ..] :: Array (Int,Int) Clover

  -- 無限ループさせる（終了条件は、どちらかが勝った時）
  (resultField, turn) <- playing emptyField picts Green font
  -- 無限ループさせる（終了条件は、クリックした時）
  gameOver resultField picts font turn

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update picts font Play


main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
  clearColor white
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"
  font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
  update [gCloverPict, rCloverPict] font Opening
