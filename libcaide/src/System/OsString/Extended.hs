{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.OsString.Extended(
    module System.OsString
) where

import Data.String (IsString(..))
import System.OsString

instance IsString OsString where
  fromString = unsafeEncodeUtf
