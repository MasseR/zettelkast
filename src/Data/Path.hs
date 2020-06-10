{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Path where
-- https://github.com/Gabriel439/Haskell-Turtle-Library/issues/54

import Control.Category
import Data.Text
       (Text)
import qualified Data.Text as T
import Prelude hiding
       (id, (.))

import Data.List (foldl')

import qualified System.FilePath.Posix as FilePath

data Root
data Dir
data File

data Path a b where
  Nil ::                            Path a a
  ConsRoot ::        Path a Root -> Path a Dir
  ConsDir ::  Text -> Path a Dir -> Path a Dir
  ConsFile :: Text -> Path a Dir -> Path a File

deriving instance Show (Path a b)

instance Category Path where
  id = Nil
  p1 . p2 =
    case p1 of
         Nil -> p2
         ConsRoot p1' -> ConsRoot (p1' . p2)
         ConsDir str p1' -> ConsDir str (p1' . p2)
         ConsFile str p1' -> ConsFile str (p1' . p2)

root :: Path Root Dir
root = ConsRoot Nil

dir :: Text -> Path Dir Dir
dir p = ConsDir p Nil

file :: Text -> Path Dir File
file p = ConsFile p Nil


toPosixPath :: Path a b -> FilePath
toPosixPath = \case
  Nil -> mempty
  ConsRoot p -> "/" <> toPosixPath p
  ConsDir path p -> toPosixPath p FilePath.</> T.unpack path
  ConsFile path p -> toPosixPath p FilePath.</> T.unpack path

-- Not entirely happy with this. It only supports root paths
fromPosixPath :: FilePath -> Path Root Dir
fromPosixPath = foldl' go root . FilePath.splitDirectories
  where
    go :: Path Root Dir -> FilePath -> Path Root Dir
    go _ "/" = root
    go p path = dir (T.pack path) . p

(./) :: Path a b -> Path b c -> Path a c
(./) = (>>>)
