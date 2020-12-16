{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: (c) 2020 Chris Upshaw
-- SPDX-License-Identifier: MIT
-- Maintainer: Chris Upshaw <chrisaupshaw@gmail.com>
--
-- See README for more info
module FftTimed where

-- ( projectName,
--   naiveFT,
--   radix_2_dit,
--   skipSplit,
-- )

-- import Vector.Unboxed.Unboxed as V

-- import Debug.Trace

import Data.Complex as C
{-fromList,-}

import Data.Vector.Unboxed as V
import Debug.SimpleReflect hiding (d, k, v, x)
import FftTimed.DydicRootOfUnity ((!!), dZetaV)
import FftTimed.RomanFft (romanFftD, romanFftU)
import Relude hiding (Show (..), fromList, length)
import qualified Relude (Show (..), length)
import qualified Text.Show as T

projectName :: String
projectName = "fft-timed"

romanFftOnV :: Vector (Complex Double) -> Vector (Complex Double)
romanFftOnV = V.fromList . fst . romanFftU . V.toList

romanFftDOnV :: Vector (Complex Double) -> Vector (Complex Double)
romanFftDOnV = V.fromList . fst . romanFftD . V.toList

class (Floating a, Unbox a) => Zeta a where
  rootOfUnity :: Int -> Int -> a

instance Zeta (Complex Double) where
  rootOfUnity ix bigN = cis ((-2 * pi * fromIntegral ix) / fromIntegral bigN)

-- instance Zeta Expr where
--   rootOfUnity = fun "rootOfUnity"

zetaVect bigN = V.generate bigN $ \ix -> rootOfUnity ix bigN

naiveFT :: Zeta a => Vector a -> Vector a
naiveFT v = generate bigN $ \ix -> V.sum $ V.imap (mkEntry ix) v
  where
    mkEntry ix ix' x = x * (zetaVect' ! (ix * ix' `rem` bigN))
    bigN = V.length v
    zetaVect' = zetaVect bigN

dNaiveFT :: Vector (Complex Double) -> Vector (Complex Double)
dNaiveFT v = generate bigN $ \ix -> V.sum $ V.imap (mkEntry ix) v
  where
    mkEntry ix ix' x = x * (zetaVect' ! (ix * ix' `rem` bigN))
    bigN = V.length v
    zetaVect' = dZetaV !! bigN
