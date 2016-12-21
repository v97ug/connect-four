module Play where

import FreeGame
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad
import qualified Debug.Trace as D

data Clover = Empty | GreenClover | RedClover deriving (Show, Eq, Ord, Ix, Enum)
data Turn = Green | Red deriving (Show, Eq)
-- type Field = [[Clover]]
type Field = Array (Int,Int) Clover
type Plot = (Double, Double) -- 描画するときの座標

-- 返り値は最終的なField
playing :: Field -> [Bitmap] -> Turn -> Font -> Int -> Game (Field,Turn)
playing field picts turn font fieldLen = do
  pos <- mousePosition
  l <- mouseDownL

  let maybeField = putClover l pos field turn fieldLen :: Maybe Field
      newField = fromMaybe field maybeField :: Field
      newCloverField = toCloverField newField :: [[Clover]]
      winnerClover = judgeFour newCloverField :: Maybe Clover
      newTurn = if isJust maybeField && isNothing winnerClover then toggle turn else turn
      isNewScene = isJust winnerClover :: Bool

  drawGrid -- 盤の描画
  drawClovers newField picts

  tick -- これ絶対必要
  if isNewScene then return (newField, newTurn) else playing newField picts newTurn font fieldLen

toCloverField :: Field -> [[Clover]]
toCloverField field =
  let (_minTuple, (fieldLen_1, _)) = bounds field
  in eachSlice (fieldLen_1 + 1) $ elems field :: [[Clover]] -- fieldLenは、-1されている

--TODO turnも必要
gameOver :: Field -> [Bitmap] -> Font -> Turn -> Game ()
gameOver field picts font turn = do
  drawGrid -- 盤の描画
  drawClovers field picts
  translate (V2 80 200) . color black $ text font 40 (show turn ++ "の勝ちです。")
  translate (V2 80 300) . color black $ text font 40 "もういちど、あそぶときは\n画面をクリックしてくださいっ。"
  l <- mouseDownL
  let fieldLen = 8 :: Int
      emptyField = replicate fieldLen $ replicate fieldLen Empty
  tick
  unless l $  gameOver field picts font turn

putClover :: Bool -> Vec2 -> Field-> Turn -> Int -> Maybe Field
putClover isPut (V2 x y) oldField turn fieldLen
  | not isPut = Nothing
  | fieldLen <= row' = Nothing
  | fieldLen <= column' = Nothing
  | oldField ! (row', column') /= Empty = Nothing
  | otherwise = Just (
      oldField // [((row', column'), makeClover turn)]
      -- take row' oldField
        -- ++ [take column' oneLine ++ [makeClover turn] ++ drop (column'+1) oneLine]
        -- ++ drop (row' + 1) oldField
        )
  where
    row' =  floor y `div` 50
    column' = floor x `div` 50
    -- oneLine = oldField !! row'

toggle :: Turn -> Turn
toggle Red = Green
toggle Green = Red

makeClover :: Turn -> Clover
makeClover Green = GreenClover
makeClover Red = RedClover

drawClovers :: Field -> [Bitmap] -> Game ()
drawClovers field picts = mapM_ (eachDraw picts) (assocs field)
  where
    -- fieldLen = length field
    -- plots = eachSlice fieldLen $ do
    --   let lenDouble = fromIntegral fieldLen :: Double
    --       plotList = [25,75..25+50*(lenDouble - 1)]
    --   y <- plotList
    --   x <- plotList
    --   return (y,x) :: [Plot]
    eachDraw :: [Bitmap] -> ((Int,Int), Clover) -> Game ()
    eachDraw picts ((row',column'), clover)= drawOneClover picts clover (x,y)
      where
        x = fromIntegral $ 25 + 50 * column' :: Double
        y = fromIntegral $ 25 + 50 * row' :: Double

drawOneClover :: [Bitmap] -> Clover -> Plot -> Game ()
drawOneClover _ Empty _ = return ()
drawOneClover  [gCloverPict, _] GreenClover (x,y) = translate (V2 x y) $ bitmap gCloverPict
drawOneClover  [_, rCloverPict] RedClover (x,y) = translate (V2 x y) $ bitmap rCloverPict

eachSlice :: Int -> [a] -> [[a]]
eachSlice _ [] = []
eachSlice n list = take n list : eachSlice n (drop n list)

judgeFour :: [[Clover]] -> Maybe Clover
judgeFour field
  | connectVertical /= [] = D.trace (show field) $ Just ((head . head) connectVertical)
  | connectHorizontal /= [] = Just ((head . head) connectHorizontal)
  | otherwise = Nothing
  where
    connectVertical = vertical field :: [[Clover]]
    connectHorizontal = horizontal field :: [[Clover]]

-- 2次元リストを転置させる
-- transpose' :: Int -> [[a]] ->[[a]]
-- transpose' n list
--   | (length . head) list < n = []
--   | otherwise = concatMap (drop (n-1) . take n) list : transpose' (n+1) list

vertical :: [[Clover]] -> [[Clover]]
vertical = horizontal . Data.List.transpose

-- Emptyを取り除く
horizontal :: [[Clover]] -> [[Clover]]
horizontal = concatMap (filter (\x -> 4 <= length x) . filter (\x -> head x /= Empty) . group)

-- 盤の描画
drawGrid :: Game ()
drawGrid = forM_ [0,50..50*8] $ \x -> do
  color blue $ line [V2 0 x, V2 (8*50) x]
  color blue $ line [V2 x 0, V2 x (8*50)]
