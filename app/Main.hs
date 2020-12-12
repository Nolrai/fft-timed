{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
    getInput,
    scaledVector,
  )
where

import Codec.Wav as W
import Control.DeepSeq
import Data.Array.Unboxed as A
import Data.Audio
import Data.Complex (Complex ((:+)), Complex, magnitude)
import Data.Vector.Unboxed as V
import FftTimed (naiveFT, projectName)
import FftTimed.IO (getInput, scaledVector)
import System.IO (hPrint, openFile)

-- import System.IO (IOMode (WriteMode), hPrint, openFile)

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
    outHandle <- ft `deepseq` openFile outFileName WriteMode
    (hPrint outHandle . magnitude) `V.mapM_` ft
    putStrLn $ "Wrote File: " <> outFileName

body :: Int -> UArray Int Int16 -> Vector (Complex Double)
body channelNumber = naiveFT . V.take 1000 . scaledVector channelNumber
