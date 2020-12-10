-- |
-- Copyright: (c) 2020 Chris Upshaw
-- SPDX-License-Identifier: MIT
-- Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>
--
-- See README for more info
module FftTimed
  ( projectName,
    naiveFT,
  )
where

import Data.Complex as C
import Data.Vector.Unboxed as V

projectName :: String
projectName = "fft-timed"

zetaVect :: Int -> Vector (Complex Double)
zetaVect bigN = V.generate bigN $ \ix -> cis ((2 * pi * fromIntegral ix) / fromIntegral bigN)

naiveFT :: Vector (Complex Double) -> Vector (Complex Double)
naiveFT v = generate bigN $ \ix -> V.sum $ V.imap (mkEntry ix) v
  where
    mkEntry ix ix' x = x * (zetaVect' ! (ix * ix' `rem` bigN))
    bigN = V.length v
    zetaVect' = zetaVect bigN
