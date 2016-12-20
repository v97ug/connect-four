module Play where

import FreeGame
import Data.List
import Data.Array

data Clover = Empty | GreenClover | RedClover deriving (Show, Eq, Ord, Ix, Enum)
data Turn = Green | Red deriving (Show, Eq)
type Field = [[Clover]]
type Plot = (Double, Double)

putClover :: Bool -> Vec2 -> Field -> Turn -> Maybe Field
putClover isPut (V2 x y) oldField turn
  | not isPut = Nothing
  | length oldField <= row' = Nothing
  | length (head oldField) <= column' = Nothing
  | oldField !! row' !! column' /= Empty = Nothing
  | otherwise = Just (
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

judgeFour :: Field -> Maybe Clover
judgeFour field
  | connectVertical /= [] = Just ((head . head) connectVertical)
  | connectHorizontal /= [] = Just ((head . head) connectVertical)
  | otherwise = Nothing
  where
    connectVertical = vertical field :: Field
    connectHorizontal = horizontal field :: Field

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
