module Game
    ( Game(..)
    , GameState(..)
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