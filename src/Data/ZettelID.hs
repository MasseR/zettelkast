{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.ZettelID where

import Text.Read
       (readMaybe)

import Data.Path

import Data.Text
       (Text)
import qualified Data.Text as T

import Text.Printf
       (printf)

import Data.Set
       (Set)
import qualified Data.Set as Set

import Data.Monoid
       (First(..))

import Data.Time
       (Day, defaultTimeLocale)
import Data.Time.Format
       (formatTime, parseTimeM)

-- | Uniquely identifying zettel
data ZettelID
  = ZettelID { day :: Day -- ^ The day when it was authored
             , counter :: Int -- ^ Increasing counter
             }
  deriving (Show, Eq, Ord)

-- | Render a zettel id as a text
--
-- The rendered id is "%y%m%d0000" where the latter number is the counter.
-- For example the second zettel for 11.6.2020 would be "2006110002"
render :: ZettelID -> Text
render ZettelID{..} = T.pack (dayFormat <> counterFormat)
  where
    dayFormat = formatTime defaultTimeLocale "%y%m%d" day
    counterFormat = printf "%03d" counter

-- | Parse the rendered zetteled id back into a 'ZettelID'
--
-- See 'render' for the format
parse :: Text -> Maybe ZettelID
parse path = ZettelID <$> dayParse path <*> counterParse path
  where
    dayParse = parseTimeM False defaultTimeLocale "%y%m%d" . T.unpack . T.take 6
    counterParse = readMaybe . T.unpack . T.drop 6 . T.takeWhile (/= '.')

-- | Convert a zettel id into a filename
toPath :: ZettelID -> Path Dir File
toPath zid = file (render zid <> ".md")

-- | Parse a filename into a zettel id
fromPath :: Path a File -> Maybe ZettelID
fromPath = \case
  ConsFile path _ -> parse (T.dropEnd 2 path)
  Nil -> Nothing

-- | Create a new unique zettel id
createZettelID :: Set ZettelID -> Day -> Maybe ZettelID
createZettelID history today = getFirst (foldMap firstFree zettelIDs)
  where
    firstFree :: ZettelID -> First ZettelID
    firstFree zid | zid `Set.member` history = First Nothing
                  | otherwise = First (Just zid)
    zettelIDs :: [ZettelID]
    zettelIDs = ZettelID today <$> [0..]
