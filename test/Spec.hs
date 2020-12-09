module Main (main) where

import FftTimed (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
