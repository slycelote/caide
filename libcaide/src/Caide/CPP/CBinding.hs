{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Caide.CPP.CBinding(
      inlineLibraryCode
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import qualified Data.Text as T
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString)


foreign import ccall unsafe "cwrapper.h inlineCppCode"
    c_inline_code  :: CString -> Ptr CString -> CInt -> Ptr CString -> CInt -> CInt
                   -> Ptr CString -> CInt -> CString -> IO CInt

inlineLibraryCode :: FilePath -> [T.Text] -> [T.Text] -> Int -> [FilePath] -> FilePath -> IO Int
inlineLibraryCode tempDir clangOptions macrosToKeep maxConsequentEmptyLines cppFiles outputFile =
    withCString (encodeString tempDir)                     $ \tempDir' ->
    withArrayOfStrings (map T.unpack clangOptions)         $ \clangOptions' ->
    withArrayOfStrings (map T.unpack macrosToKeep)         $ \macrosToKeep' ->
    withArrayOfStrings (map encodeString cppFiles)         $ \cppFiles' ->
    withCString (encodeString outputFile)                  $ \outputFile' ->
    fromIntegral <$> c_inline_code tempDir' clangOptions' (len clangOptions)
                                   macrosToKeep' (len macrosToKeep) (fromIntegral maxConsequentEmptyLines)
                                   cppFiles' (len cppFiles) outputFile'

withArrayOfStrings :: [String] -> (Ptr CString -> IO a) -> IO a
withArrayOfStrings xs m = withMany withCString xs $ \ps -> withArray ps m

len :: Num n => [a] -> n
len = fromIntegral . length

