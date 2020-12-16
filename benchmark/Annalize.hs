{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Annalize
  ( annalize,
  )
where

import Control.Monad as M
import Criterion.Types as C
import Data.Aeson
import Data.Aeson.Types
import qualified Data.List as List
import Data.Map as Map
import Data.Vector.Unboxed as V
import Graphics.EasyPlot as E
import Statistics.Regression
import Statistics.Types
import Text.Printf
import Vector.Unboxed.Unboxed as UV

data RecordFile
  = RecordFile
      { rf_library :: String,
        rf_version :: String,
        rf_reports :: V.Vector Report
      }
  deriving stock (Show)

instance FromJSON RecordFile where
  parseJSON = withArray "RecordFile" $
    \v ->
      do
        Just rf_library <- parseJSON (v V.! 0)
        Just rf_version <- parseJSON (v V.! 1)
        rf_reports <- withArray "Report Array" parseReports (v V.! 2)
        pure RecordFile {..}

parseReports :: Array -> Parser (V.Vector Report)
parseReports = V.mapM parseJSON

annalize :: FilePath -> IO ()
annalize path =
  do
    (rf_ :: Either String RecordFile) <- eitherDecodeFileStrict path
    case rf_ of
      Left str -> die str
      Right rf ->
        do
          let runs :: Map String [(Rat, Rat)] =
                toRuns $ rf_reports rf
          let loglogruns = fmap toLogLog <$> runs
          let loglogGraphs = mkGraphs loglogruns
          let fitsOnLogLog = fit loglogruns
          let fitGraphs = mkFitGraphs $ fst `fmap` fitsOnLogLog
          printData fitsOnLogLog
          let monomialFits = mkFitGraphs $ (expExp . fst) `fmap` fitsOnLogLog
          let monomialOptions = [Range (10 ^ 3) (10 ^ 4), Step (10 ^ 3)]
          myPlot (loglogGraphs <> fitGraphs [Range 3 4]) "loglog.png"
          myPlot (mkGraphs runs <> monomialFits monomialOptions) "raw.png"
          pure ()

myPlot x path = plot X11 x >> plot (PNG path) x

-- convert a linear fit on the loglog graph to a monomial fit on the original graph
expExp :: Line Rat -> Monomial Rat
expExp Line {..} = Monomial {multiplier = 10 ** yIntercept, power = slope}

data Line t = Line {slope :: t, yIntercept :: t}

data Monomial t = Monomial {multiplier :: t, power :: t}

class At t where
  type X t
  at :: t -> X t -> X t

instance At (Line Rat) where
  type X (Line Rat) = Rat
  Line {..} `at` x = slope * x + yIntercept

instance At (Monomial Rat) where
  type X (Monomial Rat) = Rat
  Monomial {..} `at` x = multiplier * (x ** power)

printData :: Map String (Line Rat, Rat) -> IO ()
printData m =
  ( \(name, (Line {..}, rSquare)) ->
      printf "%s: ~ %.2f * n ^ %.2f with R^2 = %1.3f\n" name (10 ** yIntercept) slope rSquare
  )
    `M.mapM_` Map.toList m

fit ::
  Map String [(Rat, Rat)] ->
  Map String (Line Rat, Rat)
fit m = go <$> m
  where
    go l =
      let (xs, ys) = UV.unzip (UV.fromList l)
       in let (UV.toList -> [slope, yIntercept], rSquare) = olsRegress [xs] ys
           in (Line {..}, rSquare)

toLogLog :: (Rat, Rat) -> (Rat, Rat)
toLogLog (x, y) = let f = logBase 10 in (f x, f y)

mkFitGraphs ::
  (At fun, X fun ~ Rat) =>
  Map String fun ->
  [Option2D Rat Rat] ->
  [Graph2D Rat Rat]
mkFitGraphs fits option2D =
  go <$> List.zip (Color <$> [Red, Blue, Magenta]) (Map.toList fits)
  where
    go (color, (name, fit)) =
      Function2D
        [Title ("fit for " <> name), color, Style Lines]
        option2D
        (fit `at`)

mkGraphs :: Map String [(Rat, Rat)] -> [Graph2D Rat Rat]
mkGraphs runs =
  go <$> List.zip (Color <$> [Red, Blue, Magenta]) (Map.toList runs)
  where
    go :: (E.Option, (String, [(Rat, Rat)])) -> Graph2D Rat Rat
    go (color, (name, run)) =
      Data2D [Title name, color, Style Points] [] run

onPair :: (a -> b) -> (a, a) -> (b, b)
onPair f (x, y) = (f x, f y)

toRuns :: V.Vector Report -> Map String [(Rat, Rat)]
toRuns =
  fmap Map.toList . splitIntoRuns . changeUnits . toMap . fmap toTuple

changeUnits :: Map k Rat -> Map k Rat
changeUnits = fmap (* 10 ** 6.0)

splitIntoRuns ::
  forall a b c.
  (Ord a, Ord b, Show a, Show b, Show c) =>
  Map (a, b) c ->
  Map b (Map a c)
splitIntoRuns =
  Map.foldlWithKey'
    addItemToSubMap
    mempty

addItemToSubMap ::
  forall a b c.
  (Ord a, Ord b, Show a, Show b, Show c) =>
  Map b (Map a c) ->
  (a, b) ->
  c ->
  Map b (Map a c)
addItemToSubMap soFar (a, b) c =
  insertWith
    (unionWithKey err)
    b
    (Map.singleton a c :: Map a c)
    soFar
  where
    err _one _two =
      error $
        "trying to overwrite " <> show a
          <> " in column "
          <> show b

toMap :: (Ord a, Ord b) => V.Vector (a, b, c) -> Map (a, b) c
toMap = Map.fromList . V.toList . fmap (\(a, b, c) -> ((a, b), c))

toTuple :: Report -> (Rat, String, Rat)
toTuple Report {..} =
  case reads (List.drop 1 n') of
    [(n, "")] -> (fromInteger . toInteger $ n, name, mean)
    _ -> error $ "n' is " <> show n'
  where
    (name, n') = List.break (== '/') reportName
    mean = estPoint . anMean $ reportAnalysis
