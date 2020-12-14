{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reified
  ( Reified (..),
    reifiedFunctions,
  )
where

import Control.DeepSeq
import Criterion.Main (Benchmarkable, nf)
import Data.Complex
import Data.Vector as V
import FftTimed
import System.Console.Argument
import System.Console.Command

data Reified where
  Reified ::
    String ->
    String ->
    (Vector (Complex Double) -> Vector (Complex Double)) ->
    Reified

reifiedFunctions :: [Reified]
reifiedFunctions =
  [ Reified
      "naiveFT"
      "Just multpiply and sum."
      (naiveFT :: Vector (Complex Double) -> Vector (Complex Double)),
    Reified
      "radix_2_dit"
      "basic recursive algorithem, only works for n a power of 2"
      (radix_2_dit :: Vector (Complex Double) -> Vector (Complex Double)),
    Reified
      "romanFft"
      "as described at https://ro-che.info/articles/2015-12-04-fft"
      (romanFftOnV :: Vector (Complex Double) -> Vector (Complex Double))
  ]
