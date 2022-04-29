module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event (..))

-- Drawing UI

main :: IO ()
main =
  play
    ( InWindow
        "LambdaBridgeUI"
        (800, 600)
        (0, 0)
    )
    white
    -- Simulation frame rate
    20
    initWorld
    draw
    event
    step

draw :: World -> Picture
draw _world = mempty

event :: Event -> World -> World
event _event world = world

-- Logic

data World = World

initWorld :: World
initWorld = World

step :: Float -> World -> World
step _dt w = w


