module Main where

import Play

import FreeGame
import Opening
import HowToPlay
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
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"

  let playPicts = [gCloverPict, rCloverPict]
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
    (index,newGen) = randomR (0, length list - 1) gen :: (Int, StdGen)
  in list !! index : shuffle newGen (take index list ++ drop (index + 1) list)


main :: IO (Maybe())
main = do
  gen <- newStdGen
  runGame Windowed (Box (V2 0 0) (V2 800 800)) $ do
    setTitle "くろーばーならべ"
    clearColor white
    font <- loadFont "font/jk-go-m-1/JKG-M_3.ttf"
    update font gen Opening
