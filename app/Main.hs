module Main where

import Lib
import FreeGame

data Clover = Empty | GreenClover | RedClover
type Field = [[Clover]]

update :: Field -> Game ()
update field = do
  tick -- これ絶対必要
  forM_ [0,50..50*8] $ \x -> do
    color blue $ line [V2 0 x, V2 (8*50) x]
    color blue $ line [V2 x 0, V2 x (8*50)]

  escape <- keyPress KeyEscape
  unless escape $ update field

main :: IO (Maybe())
main = runGame Windowed (Box (V2 0 0) (V2 1200 800)) $ do
  clearColor white
  let emptyField = replicate 8 $ replicate 8 Empty
  update emptyField
