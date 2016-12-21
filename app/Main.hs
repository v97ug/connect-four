module Main where

import Lib
import Play

import FreeGame
import Opening
import HowToPlay
import Data.Array

data Scene = Opening | HowToPlay | Play

update :: [Bitmap] -> Font -> Scene -> Game ()
update picts font Opening = do
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  opening picts font
  update picts font HowToPlay

update picts font HowToPlay = do
  howToPlay picts font
  update picts font Play

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
