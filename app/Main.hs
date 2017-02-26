module Main where

import Lib
import Play

import FreeGame
import Opening
import HowToPlay
import Data.Array
import System.Random

data Scene = Opening | HowToPlay | Play

update :: Font -> StdGen -> Scene -> Game ()
update font gen Opening = do
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  opening font
  update font gen HowToPlay

update font gen HowToPlay = do
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  howToPlay font
  update font gen Play

update font gen Play = do
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"

  let playPicts = [gCloverPict, rCloverPict]
      fieldLen = 8 :: Int
      indexTuple = (,) <$> indexRange <*> indexRange
        where indexRange = [0..fieldLen-1]
      fieldList = shuffle gen $ replicate numBlock Block ++ replicate (fieldLen * fieldLen - numBlock) Empty :: [FieldState]
        where numBlock = 10
      field =  array ((0,0), (fieldLen-1, fieldLen-1)) $ zip indexTuple fieldList :: Array (Int,Int) FieldState

  -- 無限ループさせる（終了条件は、どちらかが勝った時）
  (resultField, turn) <- playing field playPicts Green font
  -- 無限ループさせる（終了条件は、クリックした時）
  gameOver resultField playPicts font turn

  tick -- これ絶対必要
  update font gen Play

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle gen list =
  let
    (index,newGen) = randomR (0, length list - 1) gen :: (Int, StdGen)
  in list !! index : shuffle newGen (take index list ++ drop (index + 1) list)


main :: IO (Maybe())
main = do
  gen <- newStdGen
  runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
    clearColor white
    font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
    update font gen Opening
