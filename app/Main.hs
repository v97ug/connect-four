module Main where

import Lib
import Play

import FreeGame
import Opening
import HowToPlay
import Data.Array

data Scene = Opening | HowToPlay | Play

update :: Font -> Scene -> Game ()
update font Opening = do
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  opening font
  update font HowToPlay

update font HowToPlay = do
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  howToPlay font
  update font Play

update font Play = do
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"

  let playPicts = [gCloverPict, rCloverPict]
      fieldLen = 8 :: Int
      indexTuple = (,) <$> indexRange <*> indexRange
        where indexRange = [0..fieldLen-1]
      emptyField =  array ((0,0), (fieldLen-1, fieldLen-1)) $ zip indexTuple [Empty, Empty ..] :: Array (Int,Int) Clover

  -- 無限ループさせる（終了条件は、どちらかが勝った時）
  (resultField, turn) <- playing emptyField playPicts Green font
  -- 無限ループさせる（終了条件は、クリックした時）
  gameOver resultField playPicts font turn

  tick -- これ絶対必要
  update font Play


main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
  clearColor white
  font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
  update font Opening
