module Main where

import           Lib (getPrimesString)

main :: IO ()
main = putStr $ getPrimesString 1000 200
