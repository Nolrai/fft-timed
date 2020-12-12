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

radix_2_dit v = radix_2_dit_aux v (length v) 1

radix_2_dit_aux :: Vector (Complex Double) -> Int -> Int -> Vector  (Complex Double)
radix_2_dit_aux v 1 _ = v
radix_2_dit_aux v size stride = generate size (\k -> (if k `mod` 2 = 0 then fst else snd) $ pairs ! (k `div` 2))
  where
    halfSize = size `div` 2
    evens = radix_2_dit_aux v halfSize (stride * 2)
    odds = radix_2_dit_aux (drop stride v) halfSize (stride * 2)
    pairs = generate halfSize (\ k -> let t = evens ! k in (t + zetaVect' ! k, t - zetaVect' k))
    zetaVect' = zetaVect size