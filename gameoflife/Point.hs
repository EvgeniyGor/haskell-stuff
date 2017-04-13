module Point where

type Point = (Int, Int)

adjacents :: Point -> [Point]
adjacents (x, y) = map (\(a, b) -> (x + a, y + b)) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]