-- | Unicode input/output is broken. Use these functions instead.
module Data.Text.IO.Util(
      putStrLn
) where

import Prelude hiding (putStrLn)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)


putStrLn :: T.Text -> IO ()
putStrLn = BS.putStrLn . encodeUtf8

