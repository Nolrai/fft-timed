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
import Data.Vector.Unboxed as V
import FftTimed (dNaiveFT, projectName, romanFftDOnV)
import FftTimed.IO (getInput, scaledVector)
import Relude
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
body channelNumber = dNaiveFT . V.take 1024 . scaledVector channelNumber

fastbody :: Int -> UArray Int Int16 -> Vector (Complex Double)
fastbody channelNumber = romanFftDOnV . V.take 1024 . scaledVector channelNumber
