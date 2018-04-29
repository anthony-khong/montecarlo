module Pendulum where

import           Flow
import           Prelude as P

hamiltonian :: P.Floating a => a -> a -> a
hamiltonian position momentum = momentum ^ 2 / 2 - cos position

pendulumStep :: P.Floating a => (a -> a) -> (a -> a) -> a -> (a, a) -> (a, a)
pendulumStep dQ dP stepSize (position, momentum) = (momentum', position'')
  where
    position' = momentum - 0.5 * stepSize * dQ position
    momentum' = position + stepSize * dP position'
    position'' = position' - 0.5 * stepSize * dQ momentum'

simulations :: [(Double, Double)]
simulations = iterate (pendulumStep sin id 0.01) (pi / 4, 0.0)

simulatedHamiltonians :: [Double]
simulatedHamiltonians = simulations |> map (uncurry hamiltonian) |> take 5
{-
import Numeric.AD
import Data.Number.Symbolic
q1, q2, p1, p2 :: Sym a
q1 = var "q1"
q2 = var "q2"
p1 = var "p1"
p2 = var "p2"
nabla1 :: [[Sym Double]]
nabla1 = jacobian ham1 [q1, p1]
ham1 :: P.Floating a => [a] -> a
ham1 [qq1, pp1] = [0.5 * pp1^2 - cos qq1]
ham1 _ = error "Hamiltonian defined for 2 generalized co-ordinates only"
-}
