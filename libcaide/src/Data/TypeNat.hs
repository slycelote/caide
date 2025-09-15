{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Data.TypeNat(
      IsNat(natVal)
    , natVal'
    , n0
    , n1
    , n2
    , n3
    , n4
    , N0
    , N1
    , N2
    , N3
    , N4
) where

class IsNat n where
  natVal :: Word

data N0 = N0{}
instance IsNat N0 where natVal = 0
n0 :: N0
n0 = N0{}

data N1 = N1{}
instance IsNat N1 where natVal = 1
n1 :: N1
n1 = N1{}

data N2 = N2{}
instance IsNat N2 where natVal = 2
n2 :: N2
n2 = N2{}

data N3 = N3{}
instance IsNat N3 where natVal = 3
n3 :: N3
n3 = N3{}

data N4 = N4{}
instance IsNat N4 where natVal = 4
n4 :: N4
n4 = N4{}

natVal' :: forall n. IsNat n => n -> Word
natVal' _ = natVal @n
