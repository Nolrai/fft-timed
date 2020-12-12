{-# LANGUAGE TupleSections #-}

module FftTimed.IO where

import Codec.Wav as W
import Data.Array.Unboxed as A
import Data.Audio
import Data.Complex
import Data.Vector.Unboxed as V
import System.Environment

getInput :: IO (Either String (Audio Int16, FilePath))
getInput =
  do
    [inFileName, outFileName] <- getArgs
    fmap (,outFileName) <$> W.importFile inFileName

scaledVector :: Int -> UArray Int Int16 -> Vector (Complex Double)
scaledVector channelNumber sampleData = (\x -> scale x :+ 0) `V.map` rawVector
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
    scale x = fromIntegral (x - min') / fromIntegral ((max' - min') * fromIntegral arraySize)
