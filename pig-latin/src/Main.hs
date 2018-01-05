module Main where

import Tests

main :: IO ()
main = mapM_ putStrLn runTests
