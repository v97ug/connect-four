module Play
(  FieldState(..)
,  Turn(..)
,  playing
) where

import Field

import FreeGame
import Data.List
import Data.Maybe

data Turn = Green | Red deriving (Show, Eq)

-- 返り値は最終的なField
playing :: Field -> [Bitmap] -> Turn -> Font -> Game ()
playing field picts turn font = do
  pos <- mousePosition
  l <- mouseDownL

  let maybeField = putClover l pos field turn :: Maybe Field
      newField = fromMaybe field maybeField :: Field
      newCloverField = toList newField :: [[FieldState]]
      winnerClover = judgeFour newCloverField :: Maybe FieldState
      newTurn = if isJust maybeField && isNothing winnerClover then toggle turn else turn
      isNewScene = isJust winnerClover :: Bool
      isTie = Empty `notElem` getAllFieldState newField

  drawGrid newField -- 盤の描画
  drawClovers newField picts

  tick -- これ絶対必要
  if isNewScene || isTie
    then gameOver newField picts font newTurn isTie
    else playing newField picts newTurn font

-- 無限ループさせる（終了条件は、クリックした時）
gameOver :: Field -> [Bitmap] -> Font -> Turn -> Bool -> Game ()
gameOver field picts font turn isTie= do
  drawGrid field-- 盤の描画
  drawClovers field picts
  if isTie
    then translate (V2 80 200) . color black $ text font 40 "引き分けです。"
    else translate (V2 80 200) . color black $ text font 40 (show turn ++ "の勝ちです。")

  translate (V2 80 300) . color black $ text font 40 "もういちど、あそぶときは\n画面をクリックしてくださいっ。"
  l <- mouseDownL

  tick
  unless l $  gameOver field picts font turn isTie

putClover :: Bool -> Vec2 -> Field-> Turn -> Maybe Field
putClover isPut (V2 x y) field turn
  | not isPut = Nothing
  | maxIndex < row = Nothing
  | maxIndex < column = Nothing
  | field `notEmpty` (row, column) = Nothing
  | otherwise = Just $ field `changeFieldState` ((row, column), makeClover turn)
  where
    maxIndex = getHeightIndex field
    row =  floor y `div` 50
    column = floor x `div` 50

toggle :: Turn -> Turn
toggle Red = Green
toggle Green = Red

makeClover :: Turn -> FieldState
makeClover Green = GreenClover
makeClover Red = RedClover

drawClovers :: Field -> [Bitmap] -> Game ()
drawClovers field picts =
  forM_ (getAll field) $ \((row',column'), clover) ->
    let x = fromIntegral $ 25 + 50 * column' :: Double
        y = fromIntegral $ 25 + 50 * row' :: Double
    in  drawOneClover picts clover (x,y)

drawOneClover :: [Bitmap] -> FieldState -> (Double, Double) -> Game ()
drawOneClover _ Empty _ = return ()
drawOneClover _ Block (x,y) = color black $ line [V2 (x-30) (y-30), V2 (x+30) (y+30)]
drawOneClover  [gCloverPict, _] GreenClover (x,y) = translate (V2 x y) $ bitmap gCloverPict
drawOneClover  [_, rCloverPict] RedClover (x,y) = translate (V2 x y) $ bitmap rCloverPict

judgeFour :: [[FieldState]] -> Maybe FieldState
judgeFour field
  | connectVertical /= [] = Just ((head . head) connectVertical)
  | connectHorizontal /= [] = Just ((head . head) connectHorizontal)
  | otherwise = Nothing
  where
    connectVertical = vertical field :: [[FieldState]]
    connectHorizontal = horizontal field :: [[FieldState]]

vertical :: [[FieldState]] -> [[FieldState]]
vertical = horizontal . Data.List.transpose

-- TODO わかりやすく
horizontal :: [[FieldState]] -> [[FieldState]]
horizontal = concatMap (filter (\x -> 4 <= length x && (head x == RedClover || head x == GreenClover)) . group)

-- 盤の描画
drawGrid :: Field -> Game ()
drawGrid field =
  let fieldLen = fromIntegral (getHeight field)
  in
    forM_ [0,50..50*fieldLen] $ \x -> do
      color blue $ line [V2 0 x, V2 (fieldLen*50) x]
      color blue $ line [V2 x 0, V2 x (fieldLen*50)]

drawGrid' :: Int -> Int -> Int -> Game ()
drawGrid' height width size = do
  let height' = fromIntegral height :: Double
      width' = fromIntegral width :: Double
      size' = fromIntegral size :: Double
  forM_ [0, size'..size' * height'] $ \h ->
    forM_ [0, size'..size' * width'] $ \w -> do
      color blue $ line [V2 0 h, V2 (width' * size') h]
      color blue $ line [V2 w 0, V2 w (height' * size')]

drawGrid'' :: Int -> Int -> Game ()
drawGrid'' numSquare size = do
  let numSquare' = fromIntegral numSquare :: Double
      size' = fromIntegral size :: Double
  forM_ [0, size'..size' * numSquare'] $ \x -> do
    color blue $ line [V2 0 x, V2 (numSquare' * 50) x]
    color blue $ line [V2 x 0, V2 x (numSquare' * 50)]
