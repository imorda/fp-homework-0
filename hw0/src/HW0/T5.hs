module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

-- ((a ->a) -> a -> a) -> (a -> a) -> a -> a
ns :: Nat a -> Nat a
ns x f a = x f $ f a

-- ((a ->a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
nplus :: Nat a -> Nat a -> Nat a
nplus l r f a = l f $ r f a

nmult :: Nat a -> Nat a -> Nat a
nmult l r f = r (l f)

-- x -> ((a -> a) -> a -> a)
nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ n - 1

-- ((a -> a) -> a -> a) -> x
nToNum :: Num a => Nat a -> a
nToNum a = a (+ 1) 0
