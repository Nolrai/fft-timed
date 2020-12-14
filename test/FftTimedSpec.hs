{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module FftTimedSpec
  ( spec,
  )
where

import Codec.Wav as W
import Data.Audio
import Data.Complex (Complex (), magnitude)
import Data.Vector.Unboxed as V
import FftTimed (naiveFT, radix_2_dit)
import FftTimed.IO (scaledVector)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = beforeAll getV spec'

getV :: IO (Vector (Complex Double))
getV =
  do
    Audio {..} <- either die pure =<< W.importFile "/home/christopher/Downloads/flute-C4.wav"
    pure $ scaledVector channelNumber sampleData

spec' :: SpecWith (Vector (Complex Double))
spec' = do
  describe (show '
  describe (show 'radix_2_dit) $ do
    fit "doesn't change the size of the vector 1" $
      testPrefixes (\v size -> V.length (radix_2_dit (V.take size v)) `shouldBe` size)
    it "is almost equal to naiveFT"
      . testPrefixes
      $ \v size -> do
        let y = naiveFT (V.take size v)
        let x = naiveFT (V.take size v)
        let xSum = V.sum $ V.map magnitude x
        let errorSum = V.sum $ V.zipWith (\a b -> magnitude (a - b)) x y
        counterexample ("x : " <> show x <> "\ny : " <> show y) $
          floor (errorSum * 100 / xSum) `shouldBe` 0

-- Test the property for varius small powers of 2
testPrefixes p v = property $
  \(Positive (Small (n :: Int))) ->
    let size :: Int = 2 ^ n
     in (size > 0 && size <= V.length v) ==> p v size
