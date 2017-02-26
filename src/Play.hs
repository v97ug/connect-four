module Play where

import FreeGame
import Data.List
import Data.Array
import Data.Maybe

data FieldState = Empty | GreenClover | RedClover | Block deriving (Show, Eq, Ord, Ix, Enum)
data Turn = Green | Red deriving (Show, Eq)
type Field = Array (Int,Int) FieldState
type Plot = (Double, Double) -- 描画するときの座標

-- 返り値は最終的なField
playing :: Field -> [Bitmap] -> Turn -> Font -> Game (Field,Turn)
playing field picts turn font = do
  pos <- mousePosition
  l <- mouseDownL

  let maybeField = putClover l pos field turn :: Maybe Field
      newField = fromMaybe field maybeField :: Field
      newCloverField = toCloverField newField :: [[FieldState]]
      winnerClover = judgeFour newCloverField :: Maybe FieldState
      newTurn = if isJust maybeField && isNothing winnerClover then toggle turn else turn
      isNewScene = isJust winnerClover :: Bool

  drawGrid -- 盤の描画
  drawClovers newField picts

  tick -- これ絶対必要
  if isNewScene then return (newField, newTurn) else playing newField picts newTurn font

toCloverField :: Field -> [[FieldState]]
toCloverField field =
  let (_minTuple, (maxIndex, _)) = bounds field
  in eachSlice (maxIndex + 1) $ elems field :: [[FieldState]] -- fieldLenは、-1されている

gameOver :: Field -> [Bitmap] -> Font -> Turn -> Game ()
gameOver field picts font turn = do
  drawGrid -- 盤の描画
  drawClovers field picts
  translate (V2 80 200) . color black $ text font 40 (show turn ++ "の勝ちです。")
  translate (V2 80 300) . color black $ text font 40 "もういちど、あそぶときは\n画面をクリックしてくださいっ。"
  l <- mouseDownL

  tick
  unless l $  gameOver field picts font turn

putClover :: Bool -> Vec2 -> Field-> Turn -> Maybe Field
putClover isPut (V2 x y) field turn
  | not isPut = Nothing
  | maxIndex < row' = Nothing
  | maxIndex < column' = Nothing
  | field ! (row', column') /= Empty = Nothing
  | otherwise = Just ( field // [((row', column'), makeClover turn)] )
  where
    (_minTuple, (maxIndex, _)) = bounds field
    row' =  floor y `div` 50
    column' = floor x `div` 50

toggle :: Turn -> Turn
toggle Red = Green
toggle Green = Red

makeClover :: Turn -> FieldState
makeClover Green = GreenClover
makeClover Red = RedClover

drawClovers :: Field -> [Bitmap] -> Game ()
drawClovers fieldArray picts =
  forM_ (assocs fieldArray) $ \((row',column'), clover) ->
    let x = fromIntegral $ 25 + 50 * column' :: Double
        y = fromIntegral $ 25 + 50 * row' :: Double
    in  drawOneClover picts clover (x,y)

drawOneClover :: [Bitmap] -> FieldState -> Plot -> Game ()
drawOneClover _ Empty _ = return ()
drawOneClover _ Block (x,y) = color black $ line [V2 (x-30) (y-30), V2 (x+30) (y+30)]
drawOneClover  [gCloverPict, _] GreenClover (x,y) = translate (V2 x y) $ bitmap gCloverPict
drawOneClover  [_, rCloverPict] RedClover (x,y) = translate (V2 x y) $ bitmap rCloverPict

eachSlice :: Int -> [a] -> [[a]]
eachSlice _ [] = []
eachSlice n list = take n list : eachSlice n (drop n list)

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

-- Emptyを取り除く
-- TODO わかりやすく
horizontal :: [[FieldState]] -> [[FieldState]]
horizontal = concatMap (filter (\x -> 4 <= length x && (head x == RedClover || head x == GreenClover)) . group)

-- 盤の描画
drawGrid :: Game ()
drawGrid = forM_ [0,50..50*8] $ \x -> do
  color blue $ line [V2 0 x, V2 (8*50) x]
  color blue $ line [V2 x 0, V2 x (8*50)]
