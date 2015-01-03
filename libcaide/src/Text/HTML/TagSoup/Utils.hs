module Text.HTML.TagSoup.Utils (
      (~~==)
    , (~~/==)
) where

import Text.StringLike (StringLike, toString)
import Text.HTML.TagSoup

-- | Like '(~==)', but splits values of attributes on whitespace. Useful mainly for `class` attribute,
-- so that `TagOpen "table" [("class", "class1 class2")] ~~== "<table class=class1>"`.
(~~==) :: (StringLike str, TagRep t) => Tag str -> t -> Bool
tag ~~== desc = expandAttributes tag ~== expandAttributes (toTagRep desc :: Tag String)

-- | Negation of '~~=='.
(~~/==) :: (StringLike str, TagRep t) => Tag str -> t -> Bool
tag ~~/== desc = not $ tag ~~== desc

expandAttributes :: StringLike str => Tag str -> Tag String
expandAttributes (TagOpen tagName attributes) = TagOpen (toString tagName) $ concatMap expand attributes
expandAttributes t = fmap toString t

expand :: StringLike str => Attribute str -> [Attribute String]
expand (name, value) = [(toString name, v) | v <- splitOn ' ' $ toString value]

splitOn :: Char -> String -> [String]
splitOn c str = go str ([], "")
  where
    go "" (res, acc) = res `seq` res ++ [acc | not (null acc)]
    go (h:t) (res, acc) | h == c    = res `seq` go t (res ++ [acc | not (null acc)], "")
                        | otherwise = acc `seq` go t (res, acc ++ [h])
