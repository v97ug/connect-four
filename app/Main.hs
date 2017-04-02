module Main where

import Opening
import HowToPlay
import Play
import HexRGB

import FreeGame
import Data.Array
import System.Random

data Scene = Opening | HowToPlay | Play

update :: Font -> StdGen -> Scene -> Game ()
update font gen Opening = do
  backPict <- readBitmap "img/how-to-play.png"
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  opening font backPict
  update font gen HowToPlay

update font gen HowToPlay = do
  backPict <- readBitmap "img/how-to-play.png"
  -- 無限ループさせる（終了条件は、画面がクリックされた時）
  howToPlay font backPict
  update font gen Play

update font gen Play = do
  clearColor $ fromHexRGB "fff2e6"
  gCloverPict <- readBitmap "img/green-clover.png"
  rCloverPict <- readBitmap  "img/red-clover.png"
  blockPict <- readBitmap "img/block.png"

  let playPicts = [gCloverPict, rCloverPict, blockPict]
      fieldLen = 10 :: Int
      indexTuple = (,) <$> indexRange <*> indexRange
        where indexRange = [0..fieldLen-1]
      fieldList = shuffle gen $ replicate numBlock Block ++ replicate (fieldLen * fieldLen - numBlock) Empty :: [FieldState]
        where numBlock = 10
      field =  array ((0,0), (fieldLen-1, fieldLen-1)) $ zip indexTuple fieldList :: Array (Int,Int) FieldState

  -- 無限ループさせる（終了条件は、どちらかが勝った時）
  playing field playPicts Green font

  tick -- これ絶対必要
  let (_,newGen) = randomR (0,1) gen :: (Int, StdGen)
  update font newGen Play

shuffle :: StdGen -> [a] -> [a]
shuffle _ [] = []
shuffle gen list =
  let
    (idx,newGen) = randomR (0, length list - 1) gen :: (Int, StdGen)
  in list !! idx : shuffle newGen (take idx list ++ drop (idx + 1) list)


main :: IO (Maybe())
main = do
  gen <- newStdGen
  runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
    setTitle "くろーばーならべ"
    clearColor white
    font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
    update font gen Opening
