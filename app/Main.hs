{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Codec.Wav as W
import Control.DeepSeq
import Data.Array.Unboxed as A
import Data.Audio
import Data.Complex (Complex ((:+)), Complex, magnitude)
import Data.Int
import Data.Vector.Unboxed as V
import FftTimed (naiveFT, projectName)
import System.Environment
import System.IO (IOMode (WriteMode), hPrint, openFile)

main :: IO ()
main =
  do
    putStrLn (projectName <> ":")
    [inFileName, outFileName] <- getArgs
    result <- W.importFile inFileName
    case result of
      Left err -> putStrLn err
      Right Audio {..} ->
        do
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
body channelNumber sampleData = naiveFT . V.take 1000 $ (:+ 0) `V.map` scaledVector
  where
    start :: Int
    end :: Int
    arraySize :: Int
    (start, end) = A.bounds (sampleData :: UArray Int Int16)
    arraySize = (end - start) `div` channelNumber
    getSample :: Int -> Int16
    getSample ix = sampleData A.! (ix * channelNumber + start)
    rawVector :: Vector Int16
    rawVector = V.generate (arraySize - 1) getSample
    max' = V.maximum rawVector
    min' = V.minimum rawVector
    scaledVector :: Vector Double
    scaledVector = (\x -> fromIntegral (x - min') / (fromIntegral (max' - min') * fromIntegral arraySize)) `V.map` rawVector
