module Main where

import qualified Data.Vector      as V
import qualified Ex1
import qualified Stats            as S
import           Test.Tasty
import           Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Exp. sample" (expSampleEqual @? "Exponential samples must be equal.")
    , testCase
        "Conditional exponential sample"
        (condExpMomentsTest Ex1.rCondExp @? "Moments do not match.")
    , testCase
        "Rejection sampling conditional exponential sample"
        (condExpMomentsTest Ex1.rCondExp' @? "Moments do not match.")
    , testCase "Gamma with only uniform sample" (gamma1MomentsTest @? "Moments do not match.")
    ]

expSampleEqual :: Bool
expSampleEqual = V.all (uncurry (==)) samplePairs
  where
    samplePairs = V.zip (S.sampleExp 10) (S.sampleExp' 10)

isClose :: (Ord a, Floating a) => a -> a -> Bool
isClose = S.isCloseWithTolerance 0 2e-1

condExpMomentsTest :: (Double -> Double -> Int -> V.Vector Double) -> Bool
condExpMomentsTest rFn = isClose (S.mean xs) expectedMean && isClose (S.var xs) expectedVar
  where
    alpha = 7
    lambda = 0.33
    xs = rFn alpha lambda 100
    expectedMean = alpha + 1.0 / lambda
    expectedVar = 1.0 / lambda ** 2

gamma1MomentsTest :: Bool
gamma1MomentsTest = isClose (S.mean xs) alpha && isClose (S.var xs) alpha
  where
    alpha = 0.333
    xs = Ex1.rGamma1 alpha 500
