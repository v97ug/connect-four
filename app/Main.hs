module Main where

import Lib
import FreeGame
import Data.Maybe
import Control.Monad
import Data.List
import Debug.Trace as D

data Clover = Empty | GreenClover | RedClover deriving (Show, Eq)
data Turn = Green | Red deriving (Show, Eq)
data Scene = Opening | Play
type Field = [[Clover]]
type Plot = (Double, Double)

putClover :: Vec2 -> Field -> Turn -> Maybe (Turn, Field)
putClover (V2 x y) oldField turn
  | length oldField <= row' = Nothing
  | length (head oldField) <= column' = Nothing
  | oldField !! row' !! column' /= Empty = Nothing
  | otherwise = Just (toggle turn,
      take row' oldField
        ++ [take column' oneLine ++ [makeClover turn] ++ drop (column'+1) oneLine]
        ++ drop (row' + 1) oldField
        )
  where
    row' =  floor y `div` 50
    column' = floor x `div` 50
    oneLine = oldField !! row'

toggle :: Turn -> Turn
toggle Red = Green
toggle Green = Red

makeClover :: Turn -> Clover
makeClover Green = GreenClover
makeClover Red = RedClover

drawClovers :: Field -> [[Plot]] -> [Bitmap] -> Game ()
drawClovers field plots picts = zipWithM_ (eachLineDraw picts) field plots
  where eachLineDraw picts = zipWithM_ (drawOneClover picts)

drawOneClover :: [Bitmap] -> Clover -> Plot -> Game ()
drawOneClover _ Empty _ = return ()
drawOneClover  [gCloverPict, _] GreenClover (x,y) = translate (V2 x y) $ bitmap gCloverPict
drawOneClover  [_, rCloverPict] RedClover (x,y) = translate (V2 x y) $ bitmap rCloverPict

judgeFour :: Field -> Bool
judgeFour field
  | vertical field /= [] = True
  | horizontal field /= [] = True
  | otherwise = False

-- 2次元リストを転置させる
transpose' :: Int -> [[a]] ->[[a]]
transpose' n list
  | (length . head) list < n = []
  | otherwise = concatMap (drop (n-1) . take n) list : transpose' (n+1) list

vertical :: Field -> Field
vertical = horizontal . transpose' 1

-- Emptyを取り除く
horizontal :: Field -> Field
horizontal = concatMap (filter (\x -> 4 <= length x) . filter (\x -> head x /= Empty) . group)

-- 盤の描画
drawGrid :: Game ()
drawGrid = forM_ [0,50..50*8] $ \x -> do
  color blue $ line [V2 0 x, V2 (8*50) x]
  color blue $ line [V2 x 0, V2 x (8*50)]

-- drawPict p = translate (V2 50 50) $ bitmap p

update :: Field -> [[Plot]] -> [Bitmap] -> Turn -> Font -> Scene -> Game ()
update field plots picts turn font Opening = do
  l <- mouseDownL

  let newScene = if l then Play else Opening

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update field plots picts turn font newScene

update field plots picts turn font Play = do
  pos <- mousePosition
  l <- mouseDownL

  let result = if l then putClover pos field turn else Nothing
      (newTurn, newField) = fromMaybe (turn,field) result

  drawGrid -- 盤の描画
  drawClovers newField plots picts
  -- drawPict pict

  when (judgeFour field) . translate (V2 80 200) . color black $ text font 40 (show turn)

  tick -- これ絶対必要
  escape <- keyPress KeyEscape
  unless escape $ update newField plots picts newTurn font Play

eachSlice :: Int -> [a] -> [[a]]
eachSlice _ [] = []
eachSlice n list = take n list : eachSlice n (drop n list)

main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 400 400)) $ do
  clearColor white
  gCloverPict <- readBitmap "img/clover1.png"
  rCloverPict <- readBitmap  "img/clover2.png"
  font <- loadFont "VL-PGothic-Regular.ttf"
  let fieldLen = 8 :: Int
      emptyField = replicate fieldLen $ replicate fieldLen Empty
      plots = do
        let lenDouble = fromIntegral fieldLen :: Double
            plotList = [25,75..25+50*(lenDouble - 1)]
        y <- plotList
        x <- plotList
        return (x,y)

  update emptyField (eachSlice fieldLen plots) [gCloverPict, rCloverPict] Green font Opening
