{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Data.ZettelGraph where

import Data.Text
       (Text)
import qualified Data.Text as T

import Text.Pandoc
-- import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Walk

import Data.Path
import Data.Zettel
import Data.ZettelID
import Data.ZettelMeta
import Data.ZettelPath

import Data.Maybe
       (catMaybes, fromMaybe)

import System.FilePath
       (takeFileName)

import System.Directory
       (listDirectory)

import Control.Lens
       (from, (^.))

zettelLinks :: ZettelRoot -> IO [(ZettelMeta, [ZettelID])]
zettelLinks base = do
  paths <- listDirectory (toPosixPath (base ^. from _ZettelRoot))
  catMaybes <$> traverse go paths
  where
    go :: FilePath -> IO (Maybe (ZettelMeta, [ZettelID]))
    go (fromPath . file . T.pack -> Just zid) = do
      zettel <- readZettel base zid
      pure $ either (const Nothing) (Just . internalLinks zid) zettel
    go _ = pure Nothing

extractLink :: Inline -> [ZettelID]
extractLink = \case
  Link _ _ (u, _) -> catMaybes [fromPath (path $ T.unpack u)]
  _ -> []
  where
    path = file . T.pack . takeFileName

internalLinks :: ZettelID -> Pandoc -> (ZettelMeta, [ZettelID])
internalLinks f p = (fromMaybe unknown (zettelMeta f p), query extractLink p)
  where
    unknown = ZettelMeta f "No title"

dot :: [(ZettelMeta, [ZettelID])] -> Text
dot links = header <> nodes <> "\n" <> edges <> footer
  where
    nodes = T.unlines (map (\(f,_) -> node f) links)
    edges = T.unlines (concatMap (\(f,ts) -> map (edge (zettelID f)) ts) links)
    node :: ZettelMeta -> Text
    node ZettelMeta{..} = render zettelID <> " [label=\""<> title <>"\"];"
    edge :: ZettelID -> ZettelID -> Text
    edge f t = render f <> " -> " <> render t <> ";"
    header = "digraph g {\n"
    footer = "}"
