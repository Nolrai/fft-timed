-- https://ro-che.info/articles/2015-12-04-fft by Roman Cheplyaka
module FftTimed.RootOfUnity
  ( U, -- abstract
    mkU,
    toComplex,
    u_pow,
    u_sqr,
  )
where

import Data.Complex

-- | U q corresponds to the complex number exp(2 i pi q)
newtype U = U Rational
  deriving (Show, Eq, Ord)

-- | Convert a U number to the equivalent complex number
toComplex :: Floating a => U -> Complex a
toComplex (U q) = mkPolar 1 (2 * pi * realToFrac q)

-- | Smart constructor for U numbers; automatically performs normalization
mkU :: Rational -> U
mkU q = U (q - realToFrac (floor q))

-- | Raise a U number to a power
u_pow :: U -> Integer -> U
u_pow (U q) p = mkU (fromIntegral p * q)

-- | Square a U number
u_sqr :: U -> U
u_sqr x = u_pow x 2
