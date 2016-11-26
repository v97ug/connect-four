module Main where

import Lib
import FreeGame
import Debug.Trace as D

data Clover = Empty | GreenClover Double Double | RedClover Double Double deriving (Show, Eq)
data Turn = Green | Red deriving (Show, Eq)
type Field = [[Clover]]

putClover :: Vec2 -> Field -> Field
putClover (V2 x y) oldField =
  let row' =  floor y `div` 50
      column' = floor x `div` 50
      oneLine = oldField !! row'
      newX = 50* fromIntegral column' + 25
      newY = 50* fromIntegral row' + 25
  in
    if oldField !! row' !! column' == Empty then
      take row' oldField
        ++ [take column' oneLine ++ [GreenClover newX newY] ++ drop column' oneLine]
        ++ drop (row' + 1) oldField
    else oldField

makeClover :: Double -> Double -> Turn -> Clover
makeClover x y Green = GreenClover x y
makeClover x y Red = RedClover x y

drawClovers :: Field -> [Bitmap] -> Game ()
drawClovers [] _ = return ()
drawClovers field picts = mapM_ eachLineDraw field
  where eachLineDraw = mapM_ (drawOneClover picts)

drawOneClover :: [Bitmap] -> Clover -> Game ()
drawOneClover _ Empty = return ()
drawOneClover  [gCloverPict, _] (GreenClover x y) = translate (V2 x y) $ bitmap gCloverPict
drawOneClover  [_, rCloverPict] (RedClover x y) = translate (V2 x y) $ bitmap rCloverPict

-- drawPict p = translate (V2 50 50) $ bitmap p

update :: Field -> [Bitmap] -> Game ()
update field picts = do
  pos <- mousePosition
  l <- mouseButtonL

  -- 盤の描画
  forM_ [0,50..50*8] $ \x -> do
    color blue $ line [V2 0 x, V2 (8*50) x]
    color blue $ line [V2 x 0, V2 x (8*50)]

  let newField = if l then putClover pos field else field
  drawClovers newField picts
  -- drawPict pict

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update newField picts

main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 400 400)) $ do
  clearColor white
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"
  let emptyField = replicate 8 $ replicate 8 Empty
  update emptyField [gCloverPict, rCloverPict]
