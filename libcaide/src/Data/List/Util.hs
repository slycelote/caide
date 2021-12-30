module Data.List.Util(
      chunksOf
) where

chunksOf :: Int -> [a] -> [[a]]
chunksOf n as = go [] as where
    go acc [] = reverse acc
    go acc xs = let (pref, suff) = splitAt n xs in
        if length pref < n then go acc suff else go (pref:acc) suff

