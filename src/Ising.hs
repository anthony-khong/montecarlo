module Ising where

import           Control.Monad.State
import           Data.Random
import           Data.Random.Source.PureMT
import qualified Data.Vector.Unboxed       as V

gridSize :: Int
gridSize = 20

exampleGrid :: Int -> V.Vector Int
exampleGrid gridSize = V.fromList $ sampleGrid (gridSize * gridSize)
  where
    sampleGrid size = evalState (replicateM size sampleSign) (pureMT 123)
    sampleSign = sample (uniform (0 :: Int) (1 :: Int)) >>= \n -> return $ 2 * n - 1

couplingConstant :: Double
couplingConstant = 1.0

boltzmannConstant :: Double
boltzmannConstant = 1.0

mu :: Double
mu = 1.0
