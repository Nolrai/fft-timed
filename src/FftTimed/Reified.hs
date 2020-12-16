{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FftTimed.Reified
  ( Reified (..),
    reifiedFunctions,
  )
where

import Data.Complex
import Data.Vector.Unboxed as V
import FftTimed
import Relude (Double, String)

data Reified where
  Reified ::
    { name :: String,
      description :: String,
      func :: Vector (Complex Double) -> Vector (Complex Double)
    } ->
    Reified

reifiedFunctions :: [Reified]
reifiedFunctions =
  [ Reified
      "naiveFT"
      "Just multpiply and sum using the standard sin/cos"
      (naiveFT :: Vector (Complex Double) -> Vector (Complex Double)),
    Reified
      "dNaiveFT"
      "Just multpiply and sum but using dydic roots."
      (naiveFT :: Vector (Complex Double) -> Vector (Complex Double)),
    Reified
      "romanFftU"
      "as described at https://ro-che.info/articles/2015-12-04-fft, using standard sin/cos"
      (romanFftOnV :: Vector (Complex Double) -> Vector (Complex Double)),
    Reified
      "romanFftD"
      "as described at https://ro-che.info/articles/2015-12-04-fft, but using dydic roots."
      (romanFftDOnV :: Vector (Complex Double) -> Vector (Complex Double))
  ]
