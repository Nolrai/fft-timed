{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
    getInput,
    scaledVector,
  )
where

import Data.Array.Unboxed as A
import Data.Audio
import Data.Complex (Complex, magnitude)
import Data.Vector as V
import FftTimed (naiveFT, projectName, radix_2_dit)
import FftTimed.IO (getInput, scaledVector)
import System.IO (hPrint, openFile)

-- import System.IO (IOMode (WriteMode), hPrint, openFile)

main :: IO ()
main =
  do
    putStrLn (projectName <> ":")
    (Audio {..}, outFileName) <- either die return =<< getInput
    putStr "sampleRate: "
    print sampleRate
    putStr "channelNumber: "
    print channelNumber
    putStr "bounds: "
    print $ bounds sampleData
    let ft = body channelNumber sampleData
    outHandle <- ft `deepseq` openFile ("naive" <> outFileName) WriteMode
    (hPrint outHandle . magnitude) `V.mapM_` ft
    putStrLn $ "Wrote File: " <> ("naive" <> outFileName)
    let fastft = fastbody channelNumber sampleData
    outHandle <- fastft `deepseq` openFile ("fast" <> outFileName) WriteMode
    (hPrint outHandle . magnitude) `V.mapM_` fastft
    putStrLn $ "Wrote File: " <> ("fast" <> outFileName)

body :: Int -> UArray Int Int16 -> Vector (Complex Double)
body channelNumber = naiveFT . V.take 1024 . scaledVector channelNumber

fastbody :: Int -> UArray Int Int16 -> Vector (Complex Double)
fastbody channelNumber = radix_2_dit . V.take 1024 . scaledVector channelNumber
