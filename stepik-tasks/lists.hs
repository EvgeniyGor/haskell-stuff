module Lists where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = sum2 zs (sum2 xs ys)
    where
        sum2 :: Num a => [a] -> [a] -> [a]
        sum2 xs [] = xs
        sum2 [] ys = ys
        sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys


groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems xs = let (h, t) = span (== head xs) xs in h : groupElems t  