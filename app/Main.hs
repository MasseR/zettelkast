{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Options.Generic

import GHC.Generics
       (Generic)

import System.Process

import System.Environment

import qualified Data.Text.IO as T

import Data.Maybe
       (fromMaybe)

import Data.Path
import Data.Zettel
       (readZettel, zettels)
import Data.ZettelGraph
import Data.ZettelID
import Data.ZettelMeta
import Data.ZettelPath

import qualified Data.Set as Set

import Data.Foldable
       (for_, traverse_)
import Data.Traversable
       (for)

import Control.Lens
       (from, (^.))

import Control.Monad.Trans.Maybe
       (MaybeT(..), runMaybeT)

import Data.Time
       (getCurrentTime, utctDay)

import System.Directory
       (createDirectoryIfMissing, setCurrentDirectory)

data Commands
  = New
  | Graph
  | Open Text
  | List
  deriving (Show, Generic)

instance ParseRecord Commands

getEditor :: IO FilePath
getEditor = fromMaybe "vim" <$> lookupEnv "EDITOR"

getZettelBase :: IO (Path Root Dir)
getZettelBase = fromPosixPath <$> getEnv "ZETTEL_ROOT"

withZettelRoot :: (ZettelRoot -> IO a) -> IO a
withZettelRoot f = do
  zettelBase <- getZettelBase
  let path = ZettelRoot zettelBase
  createDirectoryIfMissing True (toPosixPath zettelBase)
  setCurrentDirectory (toPosixPath zettelBase)
  f path

createNew :: ZettelRoot -> IO ()
createNew base = do
  editor <- getEditor
  nextZettel <- createZettelID <$> (concatSet <$> zettels base) <*> (utctDay <$> getCurrentTime)
  for_ nextZettel $ \zid ->
    callProcess editor [toPosixPath ((base ^. from _ZettelRoot) ./ toPath zid)]
  where
    concatSet = Set.foldl' (\acc -> maybe acc (`Set.insert` acc)) Set.empty

open :: ZettelRoot -> Text -> IO ()
open base name = for_ (parse name) $ \zid -> do
  editor <- getEditor
  callProcess editor [toPosixPath ((base ^. from _ZettelRoot) ./ toPath zid)]

createGraph :: ZettelRoot -> IO ()
createGraph base = do
  contents <- dot <$> zettelLinks base
  T.putStrLn contents

list :: ZettelRoot -> IO ()
list base = do
  zs <- zettels @[] base
  meta <- for zs $ \mzid -> runMaybeT $ do
    zid <- MaybeT (pure mzid)
    zettel <- MaybeT (either (const Nothing) pure <$> readZettel base zid)
    MaybeT $ pure $ zettelMeta zid zettel
  traverse_ (traverse_ (T.putStrLn . format)) meta
  where
    format :: ZettelMeta -> Text
    format ZettelMeta{..} = render zettelID <> ":\t\t" <> title

main :: IO ()
main = withZettelRoot $ \base ->
  getRecord "zettelkast" >>= \case
    New -> createNew base
    Graph -> createGraph base
    Open name -> open base name
    List -> list base
