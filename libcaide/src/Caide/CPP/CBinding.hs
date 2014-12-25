{-# LANGUAGE ForeignFunctionInterface #-}

module Caide.CPP.CBinding (
      inlineLibraryCode
    , removeUnusedCode
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)


foreign import ccall unsafe "cwrapper.h inline_code"
    c_inline_code  :: Ptr CString -> CInt -> Ptr CString -> CInt -> Ptr CString -> CInt -> CString -> IO ()

inlineLibraryCode :: [FilePath] -> [FilePath] -> [FilePath] -> FilePath -> IO ()
inlineLibraryCode cppFiles systemHeaderDirs userHeaderDirs outputFile =
    withArrayOfStrings (map encodeString cppFiles) $ \cpp ->
        withArrayOfStrings (map encodeString systemHeaderDirs) $ \systemHeaders ->
            withArrayOfStrings (map encodeString userHeaderDirs) $ \userHeaders ->
                withCString (encodeString outputFile) $ \out ->
                    c_inline_code cpp (fromIntegral $ length cppFiles) systemHeaders (fromIntegral $ length systemHeaderDirs) userHeaders (fromIntegral $ length userHeaderDirs) out

foreign import ccall unsafe "cwrapper.h remove_unused_code"
    c_remove_unused_code  :: CString -> Ptr CString -> CInt -> CString -> IO ()

removeUnusedCode :: FilePath -> [FilePath] -> FilePath -> IO ()
removeUnusedCode cppFile systemHeaderDirs outputFile =
    withCString (encodeString cppFile) $ \cpp ->
        withArrayOfStrings (map encodeString systemHeaderDirs) $ \systemHeaders ->
            withCString (encodeString outputFile) $ \out ->
                c_remove_unused_code cpp systemHeaders (fromIntegral $ length systemHeaderDirs) out


withArrayOfStrings :: [String] -> (Ptr CString -> IO a) -> IO a
withArrayOfStrings xs m = withMany withCString xs $ \ps -> withArray ps m

