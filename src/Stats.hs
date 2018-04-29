{-# LANGUAGE MultiParamTypeClasses #-}

module Stats where

import           Control.Applicative
import           Control.Monad.State
import           Data.Random
import           Data.Random.Distribution.Exponential
import           Data.Random.Source.PureMT
import qualified Data.Vector                          as V
import           Data.Word
import qualified Statistics.Sample.Histogram          as S

seed :: Word64
seed = 123

sampleStream :: (Num t, Distribution d t) => d t -> [t]
sampleStream distribution = evalState (iterateM $ sample distribution) (pureMT seed)

iterateM :: (Applicative m) => m a -> m [a]
iterateM f = liftA2 (:) f (iterateM f)

sampleVector :: (Num t, Distribution d t) => d t -> Int -> V.Vector t
sampleVector distribution size = V.fromList . take size $ sampleStream distribution

sampleExp :: Int -> V.Vector Double
sampleExp = sampleVector (Exp 1)

sampleExp' :: Int -> V.Vector Double
sampleExp' = fmap (negate . log) . sampleVector StdUniform

expHistograms :: (V.Vector Double, V.Vector Double)
expHistograms = S.histogram 30 (sampleExp 1000)

expHistograms' :: (V.Vector Double, V.Vector Double)
expHistograms' = S.histogram 30 (sampleExp' 1000)

mean :: Floating a => V.Vector a -> a
mean xs = V.sum xs / fromIntegral (V.length xs)

var :: Floating a => V.Vector a -> a
var xs = mean (fmap (** 2) xs) - mean xs ** 2

isCloseWithTolerance :: (Ord a, Floating a) => a -> a -> a -> a -> Bool
isCloseWithTolerance aTol rTol x y = abs (x - y) <= aTol + rTol * abs y

isClose :: (Ord a, Floating a) => a -> a -> Bool
isClose = isCloseWithTolerance 1e-8 1e5
