{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Csv
import Data.String (IsString (..))

-- | The name of a set of benchmark results from a single run.
newtype RunName = RunName { getRunName :: String }
  deriving (Eq, Ord, Show, FromField)

instance IsString RunName where
    fromString = RunName

-- | The name of a benchmark
newtype BenchName = BenchName { getBenchName :: String }
  deriving (Eq, Ord, Show, FromField)

data RowName
    = RowBenchName BenchName
    | RowMean
  deriving (Eq, Ord, Show)

getRowName :: RowName -> String
getRowName (RowBenchName bn) = getBenchName bn
getRowName RowMean           = "Geometric mean"

data Stats = Stats
    { statsMean, statsMeanLB, statsMeanUB :: Double
    , statsStd, statsStdLB, statsStdUB    :: Double
    }
 deriving (Show)
