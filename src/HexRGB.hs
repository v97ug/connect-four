module HexRGB (
  fromHexRGB
) where

import FreeGame

hex1 :: Char -> Integer
hex1 '0' = 0
hex1 '1' = 1
hex1 '2' = 2
hex1 '3' = 3
hex1 '4' = 4
hex1 '5' = 5
hex1 '6' = 6
hex1 '7' = 7
hex1 '8' = 8
hex1 '9' = 9
hex1 'A' = 10
hex1 'a' = 10
hex1 'B' = 11
hex1 'b' = 11
hex1 'C' = 12
hex1 'c' = 12
hex1 'D' = 13
hex1 'd' = 13
hex1 'E' = 14
hex1 'e' = 14
hex1 'F' = 15
hex1 'f' = 15

eachSlice :: Int -> [a] -> [[a]]
eachSlice _ [] = []
eachSlice n list = take n list : eachSlice n (drop n list)

hex2dec :: String -> Integer
hex2dec = foldl (\d h -> d*16 + hex1 h) 0

fromHexRGB rgbHex =
  let rgbDec = map hex2dec $ eachSlice 2 rgbHex :: [Integer]
      [red, green, blue] = map ((/255) . fromIntegral) rgbDec
  in fromRGB red green blue
