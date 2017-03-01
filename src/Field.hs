module Field where

import Data.Array

data FieldState = Empty | GreenClover | RedClover | Block deriving (Show, Eq, Ord, Ix, Enum)
type Field = Array (Int,Int) FieldState

getAllFieldState :: Field -> [FieldState]
getAllFieldState = elems

getAll :: Field -> [((Int,Int), FieldState)]
getAll = assocs

getHeight :: Field -> Int
getHeight field =
  let (_, (maxHeightIndex, _)) = bounds field
  in maxHeightIndex + 1

getHeightIndex :: Field -> Int
getHeightIndex field =
  let (_, (maxHeightIndex, _)) = bounds field
  in maxHeightIndex

getWidthIndex :: Field -> Int
getWidthIndex field =
  let (_, (_, maxWidthIndex)) = bounds field
  in maxWidthIndex

getWidth :: Field -> Int
getWidth field =
  let (_, (_, maxWidthIndex)) = bounds field
  in maxWidthIndex + 1

notEmpty :: Field -> (Int, Int) -> Bool
notEmpty field (row, column) =
  field ! (row, column) /= Empty

changeFieldState :: Field -> ((Int, Int), FieldState) -> Field
changeFieldState field ((row, column), fieldState) =
  field // [((row, column), fieldState)]

toList :: Field -> [[FieldState]]
toList field =
  let (_minTuple, (maxIndex, _)) = bounds field
  in eachSlice (maxIndex + 1) $ elems field :: [[FieldState]] -- fieldLenは、-1されている
    where
      eachSlice :: Int -> [a] -> [[a]]
      eachSlice _ [] = []
      eachSlice n list = take n list : eachSlice n (drop n list)
