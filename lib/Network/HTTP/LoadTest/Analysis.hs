{-# OPTIONS_GHC -fsimpl-tick-factor=150 #-}
{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HTTP.LoadTest.Analysis
    (
    -- * Result analysis
      Analysis(..)
    , Basic(..)
    , analyseBasic
    , analyseFull
    ) where

import Control.Monad.Trans.Except (runExceptT)
import Criterion.Analysis (SampleAnalysis, analyseSample)
import Criterion.Main (defaultConfig)
import Criterion.Monad (withConfig)
import Criterion.Types (Report(reportAnalysis), Measured(..))
import Network.HTTP.LoadTest.Types (Analysis(..), Basic(..), Summary(..))
import Statistics.Quantile (weightedAvg)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Statistics.Sample as S

analyseFull :: V.Vector Summary -> Double -> IO (Analysis SampleAnalysis)
analyseFull sumv elapsed = do
  l <- withConfig defaultConfig . runExceptT $
        analyseSample 0 "0" (G.map measuredFromSummary sumv)
  case l of
    Left e -> error e
    Right v -> return Analysis {
                 latency = reportAnalysis v
               , latency99 = weightedAvg 99 100 . G.map summElapsed $ sumv
               , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sumv
               , latValues = sumv
               , throughput = fromIntegral (G.length sumv) / elapsed
    }

measuredFromSummary :: Summary -> Measured
measuredFromSummary s = Measured (summElapsed s) 0 0 0 0 0 0 0 0 0 0

analyseBasic :: V.Vector Summary -> Double -> Analysis Basic
analyseBasic sumv elapsed = Analysis {
                      latency = Basic {
                                  mean = S.mean . G.map summElapsed $ sumv
                                , stdDev = S.stdDev . G.map summElapsed $ sumv
                                }
                    , latency99 = weightedAvg 99 100 . G.map summElapsed $ sumv
                    , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sumv
                    , latValues = sumv
                    , throughput = fromIntegral (G.length sumv) / elapsed
                    }
