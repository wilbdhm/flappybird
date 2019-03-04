module Pipe where

import Graphics.Gloss
import System.Random

type Pipe = (Float, Float)

updatePipe :: Float -> Pipe -> Pipe
updatePipe o (x, y) = (x + o, y)

drawPipe :: Pipe -> Picture
drawPipe (x, y) = translate x y $ pictures [uprect, downrect]
  where
    (rw, rh) = (30, 320)
    downrect =
        color (dark yellow) $
        rectangleUpperSolid rw rh
    uprect =
        translate 0 (-rh - 65) $
        color (dark green) $
        rectangleUpperSolid rw rh

genpipes :: IO [Pipe]
genpipes = zip (repeat $ 320 + 15) . randomRs (160, 320) <$> newStdGen