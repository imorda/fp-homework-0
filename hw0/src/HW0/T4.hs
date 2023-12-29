{-# LANGUAGE BlockArguments #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix \rec f a -> case a of
  [] -> []
  (x: xs) -> f x: rec f xs

fib :: Natural -> Natural
fib n = fst $ fix (\rec i -> case i of
  0 -> (0, 0)
  1 -> (1, 0)
  x -> let (val, prev) = rec (x - 1) in
    (val + prev, val)) n

fac :: Natural -> Natural
fac = fix \rec n -> if n /= 0 then n * rec (n - 1) else 1
