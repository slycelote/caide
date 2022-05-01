{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Caide.Parsers.LeetCodeContest(
      contestParser
) where

import Control.Monad.Extended (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Util (mapLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)

import GHC.Generics (Generic)
import Network.URI (parseURI, pathSegments, URI(uriPath, uriQuery, uriFragment))

import qualified Data.Aeson as Aeson

import Caide.Commands.ParseProblem (parseProblems)
import Caide.Parsers.Common (URL, ContestParser(..), isHostOneOf)
import Caide.Types
import Caide.Util (downloadDocument, tshow)


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


eitherDecodeText' :: Aeson.FromJSON a => Text -> Either Text a
eitherDecodeText' = mapLeft T.pack . Aeson.eitherDecode' . LBS.fromStrict . T.encodeUtf8

doParseContest :: URL -> CaideIO ()
doParseContest url = case (mbUri, mbPathSegments) of
    (Just uri, Just seg) | length seg >= 2 && seg !! (length seg - 2) == "contest" -> do
        let contestId = last seg
            apiUri = uri{uriPath = "/contest/api/info/" <> contestId <> "/", uriQuery="", uriFragment=""}
            probUrlPrefix = tshow $ apiUri{uriPath="/problems/"}
        mbDoc <- liftIO $ downloadDocument $ tshow apiUri
        case mbDoc >>= eitherDecodeText' of
            Left err -> throw err
            Right (Contest{questions}) -> parseProblems 3 $
                [ probUrlPrefix <> (title_slug prob) | prob <- questions ]

    _ -> throw "Invalid contest url"
  where
    mbUri = parseURI (T.unpack url)
    mbPathSegments = pathSegments <$> mbUri

