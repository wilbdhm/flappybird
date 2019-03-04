module Main where

import Graphics.Gloss
import Bird
import Pipe
import Game
import Debug.Trace (trace)

main :: IO ()
main = do
    let window = InWindow "Flappy Bird clone (Haskell, Gloss)" (320, 480) (100, 100)
        bgcol = makeColor 0 0.5 0.8 1
        initstate = undefined
    pipes <- genpipes
    let (ps, ps2) = splitAt 3 pipes
        ps1 = zipWith updatePipe [0,120..] ps
        initstate = Game (Bird 10 320 3 0) (ps1, ps2) 0 Paused
    play window bgcol 60 initstate render handleEvents update


render :: Game -> Picture
render (Game bird (pipes, _) score _) =
    translate (-160) (-240) $
    pictures
        [ pictures $ map drawPipe pipes
        , drawBird bird
        , translate 10 10 $ text (show score)
        ]

update :: Float -> Game -> Game
-- update _ b | trace (show b) False = undefined
update elt game@(Game bird (cps, nps@(n:ns)) _ _) =
    game { pipes = (pipes, pipesn) }
  where
    ncps = map (updatePipe (-1)) cps
    ps = dropWhile (\(x, _) -> x < -15) ncps
    (pipes, pipesn) =
        case drop 2 ps of
            [] -> (ps ++ [n], ns)
            _  -> (ps, nps)

handleEvents :: a -> Game -> Game
handleEvents _ = id