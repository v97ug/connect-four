module HowToPlay where

import FreeGame

howToPlay :: Font -> Game ()
howToPlay font = do
  l <- mouseDownL

  translate (V2 80 200) . color black $ text font 40 "あそびかた"
  translate (V2 80 300) . color black $ text font 40 "縦か横に４つのクローバーを並べると、勝ちですっ。\nただし、ななめに４つ並べても勝ちにはなりません。"

  tick -- これ絶対必要
  unless l $ howToPlay font
