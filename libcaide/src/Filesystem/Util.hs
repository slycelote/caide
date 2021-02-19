module Filesystem.Util(
      -- * Unicode input/output is broken.  Use these functions to read and write text files in UTF8.
      readTextFile
    , writeTextFile
    , appendTextFile

      -- * General filesystem utilities
    , listDir
    , copyFileToDir
    , copyTreeToDir
    , pathToString
    , pathToText
    , isExecutableFile
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Directory (getPermissions, executable)

import qualified Filesystem as F
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F


import Data.Text.Encoding.Util (tryDecodeUtf8, universalNewlineConversionOnInput, universalNewlineConversionOnOutput)


-- | Read a text file. Return @Left message@ in case of a decoding error,
-- or @Right contents@ in case of success.
readTextFile :: F.FilePath -> IO (Either T.Text T.Text)
readTextFile filePath = do
    decodeResult <- tryDecodeUtf8 <$> BS.readFile (F.encodeString filePath)
    case decodeResult of
        Left err -> return $ Left err
        Right s  -> return $ Right $ universalNewlineConversionOnInput s

writeTextFile :: F.FilePath -> T.Text -> IO ()
writeTextFile filePath text = BS.writeFile (F.encodeString filePath) .
    encodeUtf8 . universalNewlineConversionOnOutput $ text

appendTextFile :: F.FilePath -> T.Text -> IO ()
appendTextFile filePath text = BS.appendFile (F.encodeString filePath) .
    encodeUtf8 . universalNewlineConversionOnOutput $ text

pathToText :: F.FilePath -> T.Text
pathToText path = case F.toText path of
    Left  s -> s
    Right s -> s

pathToString :: F.FilePath -> String
pathToString = T.unpack . pathToText

-- | Returns (file list, directory list)
listDir :: F.FilePath -> IO ([F.FilePath], [F.FilePath])
listDir dir = do
    filesAndDirs <- F.listDirectory dir
    thisIsFile <- mapM F.isFile filesAndDirs
    thisIsDir <- mapM F.isDirectory filesAndDirs
    let files = map fst . filter snd $ zip filesAndDirs thisIsFile
        dirs  = map fst . filter snd $ zip filesAndDirs thisIsDir
    return (files, dirs)

copyFileToDir :: F.FilePath -> F.FilePath -> IO ()
copyFileToDir srcFile dstDir = F.copyFile srcFile dstFile
    where dstFile = dstDir F.</> F.filename srcFile

copyTreeToDir :: F.FilePath -> F.FilePath -> IO ()
copyTreeToDir srcTree dstDir = do
    let targetDir = dstDir F.</> F.basename srcTree
    F.createDirectory True targetDir
    (files, dirs) <- listDir srcTree
    forM_ files $ \file -> copyFileToDir file targetDir
    forM_ dirs $ \dir -> copyTreeToDir dir targetDir

isExecutableFile :: MonadIO m => F.FilePath -> m Bool
isExecutableFile path = liftIO $ do
    isFile <- F.isFile path
    if not isFile
        then return False
        else do
            permissions <- getPermissions (F.encodeString path)
            return $ executable permissions

