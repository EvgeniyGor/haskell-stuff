module Data where

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ( (x1 - x2) ^ 2 + (y1 - y2) ^ 2 )



data Bit = Zero | One deriving (Show)
data Sign = Minus | Plus deriving (Show)
data Z = Z Sign [Bit] deriving (Show)

add :: Z -> Z -> Z
add x y = intToZ (zToInt x + zToInt y)

mul :: Z -> Z -> Z
mul x y = intToZ (zToInt x * zToInt y)

intToZ :: Integer -> Z
intToZ = undefined

zToInt :: Z -> Integer
zToInt = undefined