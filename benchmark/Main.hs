{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Annalize
import Codec.Wav as W
import Criterion
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Data.Audio
import Data.Complex (Complex ((:+)))
import qualified Data.List as List
import Data.Vector.Unboxed as V
import FftTimed.IO (scaledVector)
import Options.Applicative
import Reified (Reified (..), reifiedFunctions)
import System.IO (hClose)
import System.IO.Temp

opts :: ParserInfo Mode
opts =
  info
    (parseWith defaultConfig <**> helper)
    ( fullDesc
        <> progDesc "benchmarks for FftTimed"
        <> header "header"
    )

main :: IO ()
main =
  do
    mode <- execParser opts
    putStrLn $ "running with mode = " <> show mode
    case mode of
      Run config' matchType givens ->
        case jsonFile config' of
          Just path -> runBenchmarks mode path
          Nothing -> withTempFile "." "bench.json" $
            \path handle ->
              do
                let newConfig = config' {jsonFile = Just path}
                let newMode = Run newConfig matchType givens
                hClose handle
                runBenchmarks newMode path
      _ -> runBenchmarks mode "bench.json"

runBenchmarks :: Mode -> FilePath -> IO ()
runBenchmarks mode path =
  do
    Audio {..} <- either die pure =<< W.importFile "/home/christopher/Downloads/flute-C4.wav"
    runMode mode (benchmarks (scaledVector channelNumber sampleData)) >> annalize path

benchmarks :: Vector (Complex Double) -> [Benchmark]
benchmarks v =
  (onReified v <$>) . List.take 3 $ reifiedFunctions

onReified :: Vector (Complex Double) -> Reified -> Benchmark
onReified v (Reified name _description function) =
  bgroup name $
    do
      (nScale :: Int) <- [5 .. 20]
      let n :: Int = 2 ^ nScale
      pure . bench (show n) $ nf function (V.take n v)
