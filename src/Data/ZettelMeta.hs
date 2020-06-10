{-# LANGUAGE LambdaCase #-}
module Data.ZettelMeta where

import Text.Pandoc
import Text.Pandoc.Walk

import Data.Text
       (Text)
import qualified Data.Text as T

import Data.Maybe
       (listToMaybe)

import Data.ZettelID

data ZettelMeta
  = ZettelMeta { zettelID :: ZettelID
               , title :: Text
               }
  deriving Show

extractTitle :: Block -> [Text]
extractTitle = \case
  Header 1 _attr inlines -> [T.unwords (query strings inlines)]
  _ -> []
  where
    strings = \case
      Str x -> [T.pack x]
      _ -> []

zettelMeta :: ZettelID -> Pandoc -> Maybe ZettelMeta
zettelMeta zid p = ZettelMeta zid <$> listToMaybe (query extractTitle p)
