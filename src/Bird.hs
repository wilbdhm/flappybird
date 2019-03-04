module Bird
    ( Bird(..)
    , drawBird
    , updateBird
    , changeBirdVel
    ) where

import Graphics.Gloss

data Bird =
    Bird { r  :: Float
         , y  :: Float
         , vy :: Float
         , th :: Float
         }
         deriving (Show)

drawBird :: Bird -> Picture
drawBird (Bird r y _ th) =
    translate (320 / 3) y $
    scale r r $
    rotate th $
    color (dark $ dark red) $
    polygon [(-1, -1), (1, 0), (-1, 1)]

updateBird :: Bird -> Bird
updateBird bird@(Bird _ y vy th) = bird { y = y + vy, th = vy * 0.01 }

changeBirdVel :: Float -> Bird -> Bird
changeBirdVel v bird = bird { vy = v }