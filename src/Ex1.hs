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
