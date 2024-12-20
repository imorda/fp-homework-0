module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

-- a -> ((a -> Void) -> Void)
-- a -> (a -> Void) -> Void
doubleNeg :: a -> Not (Not a)
doubleNeg f g = g f

-- (((a -> Void) -> Void) -> Void) -> a -> Void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f a = f $ doubleNeg a
