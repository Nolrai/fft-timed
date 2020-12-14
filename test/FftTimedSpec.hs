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
import Data.Vector as V
import FftTimed (naiveFT, radix_2_dit, romanFftOnV)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe (show 'naiveFT)
    $ it "doesn't change the size of the vector"
    $ property (\v -> V.length (naiveFT v :: Vector (Complex Double)) `shouldBe` V.length v)
  describe (show 'radix_2_dit) $ do
    it "doesn't change the size of the vector" $
      property (\v -> V.length (naiveFT v :: Vector (Complex Double)) `shouldBe` V.length v)
    it "is almost equal to naiveFT" $ testAlmostEqualToNaiveFT 'radix_2_dit radix_2_dit
  describe (show 'romanFftOnV) $ do
    it "doesn't change the size of the vector" $
      property (\v -> V.length (naiveFT v :: Vector (Complex Double)) `shouldBe` V.length v)
    it "is almost equal to naiveFT" $ testAlmostEqualToNaiveFT 'romanFftOnV romanFftOnV

testAlmostEqualToNaiveFT :: Show t => t -> (Vector (Complex Double) -> Vector (Complex Double)) -> Property
testAlmostEqualToNaiveFT s f =
  property $
    \v -> do
      let size = V.length v
      let nv = naiveFT v
      let fv = f v
      let nSum = sqrt $ V.sum $ V.map magnitude nv
      let errorSum = sqrt $ V.sum $ V.zipWith (\a b -> magnitude (a - b)) nv fv
      counterexample
        ( toString $
            unlines
              [ "input = " <> showT (size, V.take 10 v),
                showT s <> ": " <> showT fv,
                showT 'naiveFT <> ": " <> showT nv,
                "errorSum: " <> showT errorSum,
                "nSum: " <> showT nSum
              ]
        )
        $ errorSum `shouldSatisfy` \x -> x < nSum / 100 || x < 0.0001

showT :: Show a => a -> Text
showT = show

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary =
    do
      (Positive (Small (n :: Int8))) <- arbitrary
      let size :: Int = 2 ^ (n `rem` 5)
      V.generate size . applyFun <$> arbitrary
  shrink v =
    let size = V.length v
     in let newSize = size `div` 2
         in let shrinkItems = V.fromList <$> Prelude.filter ((size ==) . Prelude.length) (shrink (V.toList v))
             in V.take newSize v : V.drop newSize v : shrinkItems
