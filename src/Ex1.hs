module Ex1 where

import           Data.Random
import qualified Data.Vector as V
import           Flow
import           Stats       as S

{-Question 1.1-}
rCondExp :: Double -> Double -> Int -> V.Vector Double
rCondExp alpha lambda = fmap (\x -> alpha - log x / lambda) . S.sampleVector StdUniform

{-Question 1.3-}
rCondExp' :: Double -> Double -> Int -> V.Vector Double
rCondExp' alpha lambda size =
    S.sampleStream StdUniform
        |> fmap (\x -> -log x / lambda)
        |> filter (> alpha)
        |> take size
        |> V.fromList

{-Question 2.5-}
rGamma1 :: Double -> Int -> V.Vector Double
rGamma1 alpha size = V.fromList . take size $ rGammaStream alpha

rGammaStream :: Double -> [Double]
rGammaStream alpha =
    S.sampleStream StdUniform
        |> toListOfTriples
        |> fmap (\(u, v, a) -> (u ** (1 / alpha), v **(1 / (1 - alpha)), -log a))
        |> filter (\(y, z, _) -> y + z <= 1)
        |> fmap (\(y, z, t) -> t * y / (y + z))
    where
        toListOfTriples (x:y:z:xs) = (x, y, z) : toListOfTriples xs

{-Question 2.6-}
rBeta :: Double -> Double -> Int -> V.Vector Double
rBeta alpha beta size =
    gammaPairs
        |> fmap (\(x, y) -> x / (x + y))
        |> take size
        |> V.fromList
    where
        gammaPairs = interleaveList (rGammaStream alpha) (rGammaStream beta)
        interleaveList (x:_:xs) (_:y:ys) = (x, y) : interleaveList xs ys
