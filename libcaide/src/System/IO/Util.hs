module System.IO.Util(
      writeFileAtomic
) where

import Control.Exception.Base (bracketOnError)
import System.Directory (renameFile, removeFile)
import System.FilePath (splitFileName, (<.>))
import System.IO (hClose, hPutStr, openTempFile)

-- Example from https://ghc.haskell.org/trac/ghc/ticket/2298
writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic targetFile content =
  bracketOnError
    (openTempFile targetDir template)
    (\(tmpFile, tmpHandle) -> hClose tmpHandle
                           >> removeFile tmpFile)
    (\(tmpFile, tmpHandle) -> hPutStr tmpHandle content
                           >> hClose tmpHandle
                           >> renameFile tmpFile targetFile)
  where
    template = targetName <.> "tmp"
    (targetDir,targetName) = splitFileName targetFile

