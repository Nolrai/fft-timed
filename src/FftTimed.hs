{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

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

-- import Data.Vector.Unboxed as V

-- import Debug.Trace

import Data.Complex as C
import Data.Vector as V
import Debug.SimpleReflect hiding (k, v, x)
import FftTimed.RomanFft (romanFft)
import qualified Text.Show as T
import Prelude hiding (Show (..), fromList, length)
import qualified Prelude (Show {-fromList,-} (..), length)

projectName :: String
projectName = "fft-timed"

romanFftOnV :: Vector (Complex Double) -> Vector (Complex Double)
romanFftOnV = V.fromList . fst . romanFft . V.toList

class Floating a => Zeta a where
  rootOfUnity :: Int -> Int -> a

instance Zeta (Complex Double) where
  rootOfUnity ix bigN = cis ((-2 * pi * fromIntegral ix) / fromIntegral bigN)

instance Zeta Expr where
  rootOfUnity = fun "rootOfUnity"

zetaVect bigN = V.generate bigN $ \ix -> rootOfUnity ix bigN

naiveFT :: Zeta a => Vector a -> Vector a
naiveFT v = generate bigN $ \ix -> V.sum $ V.imap (mkEntry ix) v
  where
    mkEntry ix ix' x = x * (zetaVect' ! (ix * ix' `rem` bigN))
    bigN = V.length v
    zetaVect' = zetaVect bigN

radix_2_dit :: Zeta a => Vector a -> Vector a
radix_2_dit v = radix_2_dit_aux (zetaVect (length v)) (fromVector v)

data SkipSlice a = SkipSlice {vec :: Vector a, offset :: Int, stride :: Int}

instance (T.Show a) => T.Show (SkipSlice a) where
  showsPrec d ss = T.showsPrec d (toVector ss)

(.!) :: SkipSlice a -> Int -> a
SkipSlice {..} .! ix = vec ! (offset + stride * ix)

ssLength :: SkipSlice a -> Int
ssLength SkipSlice {..} = V.length vec `div` stride

toVector :: SkipSlice a -> Vector a
toVector ss@SkipSlice {..} = generate (ssLength ss) (ss .!)

fromVector :: Vector a -> SkipSlice a
fromVector vec = SkipSlice {vec = vec, offset = 0, stride = 1}

data V2 a = V2 a a
  deriving (Prelude.Show, Functor)

skipSplit :: SkipSlice a -> V2 (SkipSlice a)
skipSplit SkipSlice {..} =
  let stride2 = stride * 2
   in V2 SkipSlice {vec = vec, stride = stride2, offset = offset} SkipSlice {vec = vec, stride = stride2, offset = stride}

radix_2_dit_aux :: Zeta a => Vector a -> SkipSlice a -> Vector a
radix_2_dit_aux zetaVect' ss =
  case ssLength ss of
    1 -> fromList [ss .! 0]
    size ->
      let V2 evens odds = radix_2_dit_aux zetaVect' <$> skipSplit ss
       in let twiddle k = odds ! k * (zetaVect' ! (k + 1))
           in let low k = evens ! k - twiddle k
               in let high k = evens ! k + twiddle k
                   in let pairs = generate (size `div` 2) (\k -> (low k, high k))
                       in uninterweave pairs

uninterweave :: Vector (a, a) -> Vector a
uninterweave pairs =
  let size = V.length pairs
   in generate (size * 2) (\k -> case pairs ! (k `mod` size) of (low, high) -> if k < size then low else high)
