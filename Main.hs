module Main (main) where

import           Control.Monad    (forM, zipWithM_)
import           Data.Accessor    ((^.), (^=))
import qualified Data.Map         as Map
import           Data.Maybe       (catMaybes, fromMaybe)
import           ID3.Simple       (Tag, readTag, writeTag)
import           ID3.Type         (FrameID, frame, initFrame, textContent,
                                   updateSize)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  (takeExtension, (</>))

getFrameText :: FrameID -> Tag -> Maybe String
getFrameText frid tag =
  case tag ^. frame frid of
    Nothing -> Nothing
    Just fr -> Just (fr ^. textContent)

setFrameText :: FrameID -> String -> Tag -> Tag
setFrameText frid x tag = edit $ fromMaybe (initFrame frid) (tag ^. frame frid)
  where
    edit f = frame frid ^= Just (updateSize $ textContent ^= x $ f) $ tag

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
             let path = topdir </> name
             isDirectory <- doesDirectoryExist path
             if isDirectory
               then getRecursiveContents path
               else return [path]
  return (reverse (concat paths))

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

main :: IO ()
main = do
  mp3s <- simpleFind (\p -> takeExtension p == ".mp3") "test-dir"
  tags <- mapM readTag mp3s
  let id3s = map (setFrameText "TENC" "Vinyl") (catMaybes tags)
  zipWithM_ writeTag mp3s id3s
