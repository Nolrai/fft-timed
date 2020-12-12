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
import Data.Vector.Unboxed as V
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
      (naiveFT :: Vector (Complex Double) -> Vector (Complex Double))
  ]
