module Main where

import Lib
import FreeGame
import Data.Maybe
import Debug.Trace as D

data Clover = Empty | GreenClover Double Double | RedClover Double Double deriving (Show, Eq)
data Turn = Green | Red deriving (Show, Eq)
type Field = [[Clover]]

putClover :: Vec2 -> Field -> Turn -> Maybe (Turn, Field)
-- putClover (V2 x y) oldField turn =
--   let row' =  floor y `div` 50
--       column' = floor x `div` 50
--       oneLine = oldField !! row'
--       newX = 50* fromIntegral column' + 25
--       newY = 50* fromIntegral row' + 25
--   in -- !! で、エラーを絶対にはかないようにさせる(Maybeを使って)
--     if oldField !! row' !! column' == Empty then
--       take row' oldField
--         ++ [take column' oneLine ++ [makeClover newX newY turn] ++ drop column' oneLine]
--         ++ drop (row' + 1) oldField
--     else oldField
putClover (V2 x y) oldField turn
  | length oldField <= row' = Nothing
  | length (head oldField) <= column' = Nothing
  | oldField !! row' !! column' /= Empty = Nothing
  | otherwise = Just (toggle turn,
      take row' oldField
        ++ [take column' oneLine ++ [makeClover newX newY turn] ++ drop column' oneLine]
        ++ drop (row' + 1) oldField
        )
  where
    row' =  floor y `div` 50
    column' = floor x `div` 50
    oneLine = oldField !! row'
    newX = 50 * fromIntegral column' + 25
    newY = 50 * fromIntegral row' + 25

toggle :: Turn -> Turn
toggle Red = Green
toggle Green = Red

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

-- 盤の描画
drawGrid :: Game ()
drawGrid = forM_ [0,50..50*8] $ \x -> do
  color blue $ line [V2 0 x, V2 (8*50) x]
  color blue $ line [V2 x 0, V2 x (8*50)]

-- drawPict p = translate (V2 50 50) $ bitmap p

update :: Field -> [Bitmap] -> Turn -> Game ()
update field picts turn = do
  pos <- mousePosition
  l <- mouseButtonL

  -- 盤の描画
  drawGrid

  let result = if l then putClover pos field turn else Nothing
      (newTurn, newField) = fromMaybe (turn,field) result

  drawClovers newField picts
  -- drawPict pict

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update newField picts newTurn

main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 400 400)) $ do
  clearColor white
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"
  let emptyField = replicate 8 $ replicate 8 Empty
  update emptyField [gCloverPict, rCloverPict] Green
