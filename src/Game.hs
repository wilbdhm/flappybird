module Game
    ( Game(..)
    , GameState(..)
    , birdPipeCollide
    ) where

import Bird
import Pipe

data Game =
    Game { bird  :: Bird
         , pipes :: ([Pipe], [Pipe])
         , score :: Int
         , state :: GameState
         }

instance Show Game where
    show (Game b (ps, p:_) sc st) =
        concat [ "Game (", show b, ") ("
               , show ps, ", [", show p
               , ", ...]) ", show sc, " ", show st
               ]

data GameState = Playing
               | Paused
               | Ended
               deriving (Show)

birdPipeCollide :: Bird -> Pipe -> Bool
birdPipeCollide (Bird r y _ _) (px, py) =
    x + r > px && x - r < px + 30 && y + r > py && y - r < py - 65
  where x = 320 / 3