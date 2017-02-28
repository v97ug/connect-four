module HowToPlay where

import FreeGame

howToPlay :: Font -> Bitmap -> Game ()
howToPlay font pict = do
  l <- mouseDownL

  translate (V2 400 400) $ bitmap pict
  translate (V2 80 200) . color black $ text font 40 "あそびかた"
  translate (V2 80 300) . color black $ text font 40 "縦か横に４つのクローバーを並べると、\n勝ちですっ。\nただし、ななめに４つ並べても\n勝ちにはなりません。"

  tick -- これ絶対必要
  unless l $ howToPlay font pict
