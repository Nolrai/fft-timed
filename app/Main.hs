module Main (main) where

import FftTimed (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
