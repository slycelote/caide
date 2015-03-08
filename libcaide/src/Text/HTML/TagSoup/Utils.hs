module Text.HTML.TagSoup.Utils (
      (~==)
    , (~/=)
    , (~~==)
    , (~~/==)
    , isTagName
) where

import Control.Applicative ((<$>))
import Data.Char (toLower)

import Text.StringLike (StringLike, toString)
import qualified Text.HTML.TagSoup as S
import Text.HTML.TagSoup hiding ((~==), (~/=))

-- | Constrained version of TagSoup operator
(~==) :: StringLike str => Tag str -> String -> Bool
(~==) = (S.~==)

-- | Constrained version of TagSoup operator
(~/=) :: StringLike str => Tag str -> String -> Bool
(~/=) = (S.~/=)

-- | Like '(~==)', but splits values of attributes on whitespace. Useful mainly for `class` attribute,
-- so that `TagOpen "table" [("class", "class1 class2")] ~~== "<table class=class1>"`.
(~~==) :: StringLike str => Tag str -> String -> Bool
tag ~~== desc = (map toLower <$> expandAttributes tag) S.~== expandAttributes (map toLower <$> toTagRep desc :: Tag String)

-- | Negation of '~~=='.
(~~/==) :: StringLike str => Tag str -> String -> Bool
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

