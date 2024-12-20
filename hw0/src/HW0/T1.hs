{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso ab ba) = Iso ba ab

runIso :: (a <-> b) -> (a -> b)
runIso (Iso ab _) = ab

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso 
  (\(a, (b, c)) -> ((a, b), c))
  (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso 
  (\case
    Left a -> Left (Left a)
    Right (Left b) -> Left (Right b)
    Right (Right c) -> Right c)
  (\case
    Left (Left a) -> Left a
    Left (Right b) -> Right (Left b)
    Right c -> Right (Right c))
