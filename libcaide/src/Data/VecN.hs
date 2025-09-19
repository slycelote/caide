{-# LANGUAGE DeriveTraversable #-} -- for VecN definition
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TypeOperators #-} -- for Convertible
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-} -- for fromList

module Data.VecN(
      VecN
    , fromList
    , toList
    , Convertible(pack, unpack)
    , Vec2
    , Vec3
    , Vec4
) where

import Data.List (genericLength)

import Data.TypeNat (IsNat, natVal, N2, N3, N4)

newtype VecN n a = VecN [a]
  deriving (Functor, Foldable, Traversable)

type Vec2 a = VecN N2 a
type Vec3 a = VecN N3 a
type Vec4 a = VecN N4 a

toList :: VecN n a -> [a]
toList (VecN list) = list

fromList :: forall a n. IsNat n => [a] -> Maybe (VecN n a)
fromList as = if genericLength as == natVal @n then Just (VecN as) else Nothing

class Convertible vec container | container -> vec where
  pack :: container -> vec
  unpack :: vec -> container

instance Convertible (VecN n a) (VecN n a) where
  pack = id
  unpack = id

wrongLength :: [a] -> b
wrongLength list = error $ "Impossible happened: VecN contains " ++ show (length list) ++ " elements"

-- Equality constraints (b~a) are used for better type inference in polymorphic
-- contexts, see e.g. https://stackoverflow.com/a/11554793

instance b~a => Convertible (Vec2 a) (a, b) where
  pack (a, b) = VecN [a, b]
  unpack (VecN [a, b]) = (a, b)
  unpack (VecN list) = wrongLength list

instance (b~a, c~a) => Convertible (Vec3 a) (a, b, c) where
  pack (a, b, c) = VecN [a, b, c]
  unpack (VecN [a, b, c]) = (a, b, c)
  unpack (VecN list) = wrongLength list

instance (b~a, c~a, d~a) => Convertible (Vec4 a) (a, b, c, d) where
  pack (a, b, c, d) = VecN [a, b, c, d]
  unpack (VecN [a, b, c, d]) = (a, b, c, d)
  unpack (VecN list) = wrongLength list
