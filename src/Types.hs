{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Csv
import Data.String (IsString (..))

import qualified Statistics.Distribution        as S
import qualified Statistics.Distribution.Normal as S

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
    | RowFit
  deriving (Eq, Ord, Show)

getRowName :: RowName -> String
getRowName (RowBenchName bn) = getBenchName bn
getRowName RowMean           = "Geometric mean"
getRowName RowFit            = "=============="

data Stats = Stats
    { statsMean, statsMeanLB, statsMeanUB :: !Double
    , statsStd, statsStdLB, statsStdUB    :: !Double
    }
 deriving (Show)

type ND = S.NormalDistribution

statsND :: Stats -> ND
statsND s = S.normalDistr (statsMean s) (statsStd s)

addND :: ND -> ND -> ND
addND x y = S.normalDistr (S.mean x + S.mean y) (sqrt (S.variance x + S.variance y))

subND :: ND -> ND -> ND
subND x y = S.normalDistr (S.mean x - S.mean y) (sqrt (S.variance x + S.variance y))

scaleND :: Double -> ND -> ND
scaleND k x = S.normalDistr (k * S.mean x) (abs k * S.stdDev x)
