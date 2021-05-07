{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Caide.Parsers.CodeChefContest(
      codeChefContestParser
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (parseURI, uriPath)

import qualified Data.Aeson as Aeson

import Caide.Commands.ParseProblem (parseProblems)
import Caide.Parsers.Common (URL, ContestParser(..), isHostOneOf)
import Caide.Types
import Caide.Util (downloadDocument)

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
                 { problems :: Map.Map ProblemID JsonProblem
                 } deriving (Generic, Show)

instance Aeson.FromJSON JsonProblem
instance Aeson.FromJSON JsonContest

parseFromJson :: Text -> Either Text [ProblemID]
parseFromJson jsonText = case Aeson.eitherDecode' . LBS.fromStrict . encodeUtf8 $ jsonText of
    Left err -> throwError $ T.pack err
    Right contest -> return $ Map.keys $ problems $ contest

doParseContest :: URL -> CaideIO ()
doParseContest url = case uriPath <$> parseURI (T.unpack url) of
    Just (_:contestId) -> do
        let apiUrl = "https://www.codechef.com/api/contests/" <> T.pack contestId
            probUrlPrefix = "https://www.codechef.com/" <> T.pack contestId <> "/problems/"
        mbDoc <- liftIO $ downloadDocument apiUrl
        case mbDoc >>= parseFromJson of
            Left err      -> throw err
            Right probIds -> parseProblems 1 $ map (probUrlPrefix <> ) probIds
    _ -> throw "Invalid contest url"

