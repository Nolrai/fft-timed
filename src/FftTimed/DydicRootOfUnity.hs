{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FftTimed.DydicRootOfUnity
  ( dZetaV,
    dZetaM,
    (S.!!),
    Rat,
  )
where

import Control.Exception (assert)
import Data.Complex
import Data.Function (on)
import Data.Ratio
import Data.Stream as S
import Data.Vector.Unboxed as V
import Math.NumberTheory.Logarithms
import Relude

type Rat = Ratio Int

dZetaM :: Rat -> Complex Double
dZetaM 0 = 1 :+ 0
dZetaM r
  | r >= 1 || r < 0 = dZetaM (r - realToFrac (floor r))
  | r >= 1 % 4 = rotate $ dZetaM (r - 1 % 4)
  | otherwise =
    let step = (1 % denominator r)
     in (bisect `on` (dZeta . (r +))) step (negate step)

bisect a b = signum (a + b)

-- rotate 90* clockwise
rotate :: Num a => Complex a -> Complex a
rotate (a :+ b) = negate b :+ a

dZeta :: Rat -> Complex Double
dZeta r =
  let (top, bot) = (numerator r, denominator r)
   in let vec = dZetaV S.!! intLog2 bot
       in case vec V.!? top of
            Just c -> c
            Nothing -> error . toText $ "dZeta " <> show (r, vec)

dZetaV :: Stream (Vector (Complex Double))
dZetaV =
  do
    bigN :: Int <- S.iterate (+ 1) 0
    let size :: Int = 2 ^ bigN
    pure $ generate (assert (size > 0) size) (\ix -> dZetaM (ix % size))
