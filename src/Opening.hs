module Opening where

import FreeGame

opening :: Font -> Bitmap -> Game ()
opening font pict = do
  l <- mouseDownL

  translate (V2 400 400) $ bitmap pict
  translate (V2 80 200) . color black $ text font 40 "くろーばーならべ"
  translate (V2 80 300) . color black $ text font 40 "click to start"

  tick -- これ絶対必要
  unless l $ opening font pict
