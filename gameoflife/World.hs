module World where

import Point

type World = [Point]

height = 20
width = 80

blinker :: Point -> World
blinker (x, y) = [(x,y), (x + 1, y), (x + 2, y)]

block :: Point -> World
block (x, y) = [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)]

alives = blinker (3, 3) ++ block (0, 4)

updateWorld :: World -> World
updateWorld = map (\(a, b) -> (a + 2, b +3))

lifeCycle :: World -> World
lifeCycle [] = []
lifeCycle = lifeCycle . updateWorld