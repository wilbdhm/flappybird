module Pipe where

import Graphics.Gloss
import System.Random

type Pipe = (Float, Float)

updatePipe :: Float -> Pipe -> Pipe
updatePipe o (x, y) = (x + o, y)

drawPipe :: Pipe -> Picture
drawPipe (x, y) = pictures $ map (translate x y) [uprect, downrect]
  where
    (rw, rh) = (30, 60)
    downrect =
        color (dark yellow) $
        rectangleUpperSolid rw rh
    uprect =
        translate 0 (-20) $
        color (dark green) $
        rectangleUpperSolid rw rh

genpipes :: IO [Pipe]
genpipes = zip (repeat $ 640 + 120) . randomRs (30, 450) <$> newStdGen