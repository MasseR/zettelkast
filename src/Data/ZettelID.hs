{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ZettelID where

import Text.Read (readMaybe)

import Data.Path

import qualified Data.Text as T
import Data.Text (Text)

import Text.Printf (printf)

import Data.Set
       (Set)
import qualified Data.Set as Set

import Data.Monoid
       (First(..))

import Data.Time
       (Day, defaultTimeLocale)
import Data.Time.Format
       (formatTime, parseTimeM)

data ZettelID
  = ZettelID { day :: Day
             , counter :: Int
             }
  deriving (Show, Eq, Ord)

render :: ZettelID -> Text
render ZettelID{..} = T.pack (dayFormat <> counterFormat)
  where
    dayFormat = formatTime defaultTimeLocale "%y%m%d" day
    counterFormat = printf "%03d" counter

parse :: Text -> Maybe ZettelID
parse path = ZettelID <$> dayParse path <*> counterParse path
  where
    dayParse = parseTimeM False defaultTimeLocale "%y%m%d" . T.unpack . T.take 6
    counterParse = readMaybe . T.unpack . T.drop 6 . T.takeWhile (/= '.')

toPath :: ZettelID -> Path Dir File
toPath zid = file (render zid <> ".md")

fromPath :: Path a File -> Maybe ZettelID
fromPath = \case
  ConsFile path _ -> parse (T.dropEnd 2 path)
  Nil -> Nothing

createZettelID :: Set ZettelID -> Day -> Maybe ZettelID
createZettelID history today = getFirst (foldMap firstFree zettelIDs)
  where
    firstFree :: ZettelID -> First ZettelID
    firstFree zid | zid `Set.member` history = First Nothing
                  | otherwise = First (Just zid)
    zettelIDs :: [ZettelID]
    zettelIDs = ZettelID today <$> [0..]
