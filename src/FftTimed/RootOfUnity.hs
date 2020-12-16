-- https://ro-che.info/articles/2015-12-04-fft by Roman Cheplyaka
module FftTimed.RootOfUnity
  ( U, -- abstract
    mkU,
    toComplexU,
    toComplexD,
    uPow,
    uSqr,
  )
where

import Control.Exception
import Data.Complex
import Data.Ratio
import FftTimed.DydicRootOfUnity
import Relude

-- | U q corresponds to the complex number exp(2 i pi q)
newtype U = U Rat
  deriving (Show, Eq, Ord)

-- | Convert a U number to the equivalent complex number
toComplexU :: Floating a => U -> Complex a
toComplexU (U q) = mkPolar 1 (2 * pi * realToFrac q)

toComplexD :: U -> Complex Double
toComplexD (U q) = dZetaM q

-- | Smart constructor for U numbers; automatically performs normalization
mkU :: Rat -> U
mkU q = U (q - realToFrac (floor q))

-- | Raise a U number to a power
uPow :: U -> Integer -> U
uPow (U q) p = mkU (fromIntegral p * q)

-- | Square a U number
uSqr :: U -> U
uSqr x = uPow x 2
