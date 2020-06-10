module Data.Zettel where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory (listDirectory)

import Data.Path
import Data.ZettelID
import Data.ZettelPath

import Data.Pointed (Pointed, point)

import Text.Pandoc

zettels :: (Monoid (f (Maybe ZettelID)), Pointed f) => ZettelRoot -> IO (f (Maybe ZettelID))
zettels (ZettelRoot r) = do
  paths <- listDirectory (toPosixPath r)
  let files = map (\p -> r ./ file (T.pack p))  paths
  pure $ foldMap (point . fromPath) files

readZettel :: ZettelRoot -> ZettelID -> IO (Either PandocError Pandoc)
readZettel (ZettelRoot base) zid = do
  contents <- T.readFile (toPosixPath (base ./ toPath zid))
  runIO (readMarkdown def contents)
