module Text.HTML.TagSoup.Utils (
      (~~==)
    , (~~/==)
    , isTagName
    , mergeTextTags
) where

import Data.List (groupBy)

import Text.StringLike (StringLike, toString, strConcat)
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


isTagName :: Eq str => str -> Tag str -> Bool
isTagName name tag = isTagOpenName name tag || isTagCloseName name tag

mergeTextTags :: (StringLike str, Show str) => [Tag str] -> [Tag str]
mergeTextTags = map merge . groupBy cmp
  where
    cmp t1 t2 = isTagText t1 && isTagText t2

    merge tags@(TagText _ : _) = TagText . strConcat . map fromTagText $ tags
    merge [t] = t
    merge _ = error "mergeTextTags"

