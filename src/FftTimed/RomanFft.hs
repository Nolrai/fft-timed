{-# LANGUAGE ScopedTypeVariables #-}

-- modified from https://ro-che.info/articles/2015-12-04-fft by Roman Cheplyaka

module FftTimed.RomanFft
  ( romanFftU,
    romanFftD,
  )
where

import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.Complex
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Data.Ratio
import FftTimed.RootOfUnity
import Relude

split :: [a] -> ([a], [a])
split = foldr f ([], [])
  where
    f a (r1, r2) = (a : r2, r1)

evalFourier ::
  forall a.
  RealFloat a =>
  -- | a method for calculating roots of unity
  (U -> Complex a) ->
  -- | polynomial coefficients, starting from a_0
  [Complex a] ->
  -- | points at which to evaluate the polynomial
  [U] ->
  Writer (Sum Int) [Complex a]
evalFourier _ [] pts = return $ 0 <$ pts
evalFourier _ [c] pts = return $ c <$ pts
evalFourier toComplex coeffs pts = do
  let squares = nub $ uSqr <$> pts -- values of x^2
      (even_coeffs, odd_coeffs) = split coeffs
  even_values <- evalFourier toComplex even_coeffs squares
  odd_values <- evalFourier toComplex odd_coeffs squares
  let -- a mapping from x^2 to (A_e(x^2), A_o(x^2))
      square_map =
        Map.fromList
          . zip squares
          $ zip even_values odd_values
      -- evaluate the polynomial at a single point
      eval1 :: U -> Writer (Sum Int) (Complex a)
      eval1 x = do
        let (ye, yo) = square_map Map.! uSqr x
        let r = ye + toComplex x * yo
        tell $ Sum 2 -- this took two arithmetic operations
        return r
  mapM eval1 pts

romanFft :: RealFloat a => (U -> Complex a) -> [Complex a] -> ([Complex a], Int)
romanFft toComplex coeffs =
  second getSum
    . runWriter
    . evalFourier toComplex coeffs
    . map (uPow w)
    $ [0 .. n -1]
  where
    n :: Integral a => a
    n = genericLength coeffs
    w = mkU (-1 % n)

romanFftU = romanFft toComplexU

romanFftD = romanFft toComplexD
