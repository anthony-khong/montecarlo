module Pendulum where

import           Prelude as P

hamiltonian :: P.Floating a => a -> a -> a
hamiltonian q p = p ^ 2 / 2 - cos q

pendulumStep :: P.Floating a => (a -> a) -> (a -> a) -> a -> (a, a) -> (a, a)
pendulumStep dQ dP stepSize (q, p) = (q', p'')
  where
    p' = p - 0.5 * stepSize * dQ q
    q' = q + stepSize * dP p'
    p'' = p' - 0.5 * stepSize * dQ q'

simulations :: [(Double, Double)]
simulations = iterate (pendulumStep sin id 0.01) (pi / 4, 0.0)
