{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Caide.Parsers.LeetCodeContest(
      contestParser
) where

import Data.Either.Util (mapLeft)
import qualified Data.Text as T
import Data.Text (Text)

import GHC.Generics (Generic)
import Network.URI (parseURI, pathSegments, URI(uriPath, uriQuery, uriFragment))

import qualified Data.Aeson as Aeson

import qualified Caide.HttpClient as Http
import Caide.Parsers.Common (URL, ContestParser(..), ContestParserResult(Urls), isHostOneOf)
import Caide.Util (tshow)


isLeetCodeUrl :: URL -> Bool
isLeetCodeUrl = isHostOneOf ["leetcode.com", "www.leetcode.com", "leetcode-cn.com", "www.leetcode-cn.com"]


contestParser :: ContestParser
contestParser = ContestParser
    { contestUrlMatches = isLeetCodeUrl
    , parseContest = doParseContest
    }

newtype ProblemInContest = ProblemInContest { title_slug :: Text }
    deriving (Show, Generic)
instance Aeson.FromJSON ProblemInContest

newtype Contest = Contest { questions :: [ProblemInContest] }
    deriving (Show, Generic)
instance Aeson.FromJSON Contest


doParseContest :: Http.Client -> URL -> IO (Either Text ContestParserResult)
doParseContest client url = case (mbUri, mbPathSegments) of
    (Just uri, Just seg) | length seg >= 2 && seg !! (length seg - 2) == "contest" -> do
        let contestId = last seg
            apiUri = uri{uriPath = "/contest/api/info/" <> contestId <> "/", uriQuery="", uriFragment=""}
            probUrlPrefix = tshow $ apiUri{uriPath="/problems/"}
        mbDoc <- Http.get client apiUri
        case mbDoc >>= (mapLeft T.pack . Aeson.eitherDecode') of
            Left err -> pure $ Left err
            Right (Contest{questions}) -> pure . Right . Urls $
                [ probUrlPrefix <> (title_slug prob) | prob <- questions ]

    _ -> pure $ Left "Invalid contest url"
  where
    mbUri = parseURI (T.unpack url)
    mbPathSegments = pathSegments <$> mbUri

