{-# LANGUAGE ForeignFunctionInterface #-}

module Caide.CPP.CBinding(
      inlineLibraryCode
    , removeUnusedCode
) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)


foreign import ccall unsafe "cwrapper.h inline_code"
    c_inline_code  :: Ptr CString -> CInt -> Ptr CString -> CInt -> CString -> IO CInt

inlineLibraryCode :: [FilePath] -> [T.Text] -> FilePath -> IO Int
inlineLibraryCode cppFiles cmdLineOptions outputFile =
    withArrayOfStrings (map encodeString cppFiles)         $ \cpp ->
    withArrayOfStrings (map T.unpack cmdLineOptions)       $ \options ->
    withCString (encodeString outputFile)                  $ \out ->
    fromIntegral <$> c_inline_code cpp (len cppFiles) options (len cmdLineOptions) out

foreign import ccall unsafe "cwrapper.h remove_unused_code"
    c_remove_unused_code  :: CString -> Ptr CString -> CInt -> Ptr CString -> CInt -> CString -> IO CInt

removeUnusedCode :: FilePath -> [T.Text] -> [T.Text] -> FilePath -> IO Int
removeUnusedCode cppFile cmdLineOptions macrosToKeep outputFile =
    withCString (encodeString cppFile)                     $ \cpp ->
    withArrayOfStrings (map T.unpack cmdLineOptions)       $ \options ->
    withArrayOfStrings (map T.unpack macrosToKeep)         $ \toKeep ->
    withCString (encodeString outputFile)                  $ \out ->
    fromIntegral <$> c_remove_unused_code cpp options (len cmdLineOptions) toKeep (len macrosToKeep) out

withArrayOfStrings :: [String] -> (Ptr CString -> IO a) -> IO a
withArrayOfStrings xs m = withMany withCString xs $ \ps -> withArray ps m

len :: Num n => [a] -> n
len = fromIntegral . length

