{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Caide.Parsers.CodeChefContest(
      codeChefContestParser
) where

import Control.Monad.Extended (liftEither, orThrow, runExceptT)
import Data.Either.Util (mapLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (parseURI, uriPath)

import qualified Data.Aeson as Aeson

import qualified Caide.HttpClient as Http
import Caide.Parsers.Common (URL, ContestParser(..), ContestParserResult(Urls),
    isHostOneOf)


codeChefContestParser :: ContestParser
codeChefContestParser = ContestParser
    { contestUrlMatches = isCodeChefUrl
    , parseContest = doParseContest
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl = isHostOneOf ["codechef.com", "www.codechef.com"]

data JsonProblem = JsonProblem
                 { code :: Text
                 -- , name :: Text
                 } deriving (Generic, Show)

data JsonContest = JsonContest
                 { problems :: Map.Map Text JsonProblem
                 } deriving (Generic, Show)

instance Aeson.FromJSON JsonProblem
instance Aeson.FromJSON JsonContest


doParseContest :: Http.Client -> URL -> IO (Either Text ContestParserResult)
doParseContest client url = case uriPath <$> parseURI (T.unpack url) of
    Just (_:contestId) -> runExceptT $ do
        let apiUrl = "https://www.codechef.com/api/contests/" <> contestId
            probUrlPrefix = "https://www.codechef.com/" <> T.pack contestId <> "/problems/"
        apiUri <- parseURI apiUrl `orThrow` "Invalid API URL"
        doc <- Http.get client apiUri >>= liftEither
        contest <- liftEither $ mapLeft T.pack $ Aeson.eitherDecode' doc
        let probIds = Map.keys $ problems contest
        pure $ Urls $ map (probUrlPrefix <> ) probIds

    _ -> pure $ Left "Invalid contest url"

