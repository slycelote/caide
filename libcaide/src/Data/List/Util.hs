{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module Data.List.Util(
      chunksOf
    , chunked
) where

import Data.Maybe (fromJust)
import Data.TypeNat (IsNat, natVal)
import Data.VecN (Convertible(unpack), VecN, fromList)

chunksOf :: Int -> [a] -> [[a]]
chunksOf chunkSize as = go [] as where
    go acc [] = reverse acc
    go acc xs = let (pref, suff) = splitAt chunkSize xs in
        if length pref == chunkSize
        then go (pref:acc) suff
        else go acc suff -- reached the end

-- | Split the list into chunks of (compile-type constant) n.
-- Can return VecN n a, or a tuple of length n
chunked :: forall n container a. (IsNat n, Convertible (VecN n a) container) => [a] -> [container]
chunked as = map (unpack . fromJust . fromList) $ chunksOf chunkSize as where
    chunkSize = fromIntegral $ natVal @n

