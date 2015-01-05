{-# LANGUAGE ForeignFunctionInterface #-}

module Caide.CPP.CBinding (
      inlineLibraryCode
    , removeUnusedCode
) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)


foreign import ccall unsafe "cwrapper.h inline_code"
    c_inline_code  :: Ptr CString -> CInt -> Ptr CString -> CInt -> CString -> IO CInt

inlineLibraryCode :: [FilePath] -> [String] -> FilePath -> IO Int
inlineLibraryCode cppFiles cmdLineOptions outputFile =
    withArrayOfStrings (map encodeString cppFiles)         $ \cpp ->
    withArrayOfStrings cmdLineOptions                      $ \options ->
    withCString (encodeString outputFile)                  $ \out ->
    fromIntegral <$> c_inline_code cpp (len cppFiles) options (len cmdLineOptions) out

foreign import ccall unsafe "cwrapper.h remove_unused_code"
    c_remove_unused_code  :: CString -> Ptr CString -> CInt -> CString -> IO CInt

removeUnusedCode :: FilePath -> [String] -> FilePath -> IO Int
removeUnusedCode cppFile cmdLineOptions outputFile =
    withCString (encodeString cppFile)                     $ \cpp ->
    withArrayOfStrings cmdLineOptions                      $ \options ->
    withCString (encodeString outputFile)                  $ \out ->
    fromIntegral <$> c_remove_unused_code cpp options (len cmdLineOptions) out

withArrayOfStrings :: [String] -> (Ptr CString -> IO a) -> IO a
withArrayOfStrings xs m = withMany withCString xs $ \ps -> withArray ps m

len :: Num n => [a] -> n
len = fromIntegral . length

