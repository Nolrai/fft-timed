{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module FftTimedSpec
  ( spec,
  )
where

import Data.Complex (Complex (), magnitude)
import Data.Vector.Unboxed as V
import FftTimed (dNaiveFT, naiveFT, radix_2_dit, romanFftDOnV, romanFftOnV)
import FftTimed.Reified
import Relude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = block `Relude.mapM_` reifiedFunctions

block :: Reified -> Spec
block Reified {..} =
  describe name $ do
    it description $ property (`shouldBe` ())
    it "doesn't change the size of the vector" $
      property (\v -> V.length (func v :: Vector (Complex Double)) `shouldBe` V.length v)
    when (name /= "naiveFT") $ it "is almost equal to naiveFT" $ testAlmostEqualToNaiveFT name func

testAlmostEqualToNaiveFT :: ToText t => t -> (Vector (Complex Double) -> Vector (Complex Double)) -> Property
testAlmostEqualToNaiveFT s f =
  property $
    \v -> do
      let size = V.length v
      let nv = naiveFT v
      let fv = f v
      let nSum = sqrt $ V.sum $ V.map magnitude nv
      let errorVec = V.zipWith (\a b -> magnitude (a - b)) nv fv
      let errorSum = sqrt $ V.sum errorVec
      counterexample
        ( toString $
            unlines
              [ "input = " <> showT (size, V.take 10 v),
                toText s <> ": " <> showT fv,
                showT 'naiveFT <> ": " <> showT nv,
                "errorVec: " <> showT errorVec,
                "errorSum: " <> showT errorSum,
                "nSum: " <> showT nSum
              ]
        )
        $ errorSum `shouldSatisfy` \x -> x < nSum / 100 || x < 0.0001

-- force the result to be a Text
showT :: Show a => a -> Text
showT = show

-- this instance only produces vectors that have a length of a power of two.
instance (Unbox a, Arbitrary a) => Arbitrary (Vector a) where
  arbitrary =
    do
      (Positive (Small (n :: Int8))) <- arbitrary
      let size :: Int = 2 ^ (n `rem` 5)
      V.generate size . applyFun <$> arbitrary
  shrink v =
    let size = V.length v
     in let newSize = size `div` 2
         in let shrinkItems = V.fromList <$> Relude.filter ((size ==) . Relude.length) (shrink (V.toList v))
             in V.take newSize v : V.drop newSize v : shrinkItems
