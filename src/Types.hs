{-|
Module      : Types
Description : Defines data types
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module defines the types used by the program and the functions to access 
their fields.
-}

{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Audiobook(Audiobook)
    , Episode(Episode)
    , Podcast(Podcast)
    , generatePodcast
    , episodeUrl
    , episodeTitle
    , episodeTrackUrl
    , episodeCoverUrl
    , episodeDuration
    , audiobookTitle
    , audiobookDescription
    , audiobookCoverUrl
    , audiobookEpisodes
    , audiobook
    , baseUrl
    , pubDay
    , generatePodcastFileName
    ) where

import Data.Char ( toLower )
import Data.Time.Calendar ( Day )
import Text.Mustache
import Text.Mustache.Types ( Pair )

-- | The 'Episode' data type represents an episode of the podcast.
-- 'Episode' is an istance of 'ToMustache' typeclass.
data Episode = Episode { episodeUrl :: String
                       , episodeTitle :: String
                       , episodeTrackUrl :: String
                       , episodeCoverUrl :: String
                       , episodeDuration :: String
}
    deriving (Show)

-- | The 'Audiobook' data type represents the audiobook of the podcast.
-- 'Audiobook' is an istance of 'ToMustache' typeclass.
data Audiobook = Audiobook { audiobookTitle :: String
                           , audiobookDescription :: String
                           , audiobookCoverUrl :: String
                           , audiobookEpisodes :: [Episode]
}
    deriving (Show)


-- | The 'Podcast' data type represents the podcast.
-- 'Podcast' is an istance of 'ToMustache' typeclass.
data Podcast = Podcast { audiobook :: Audiobook
                       , baseUrl :: String
                       , pubDay :: Day
}
    deriving (Show)

toPairList :: Audiobook -> [Pair]
toPairList audiobook =
        [ "audiobook-title" ~> audiobookTitle audiobook
        , "audiobook-cover-url" ~> audiobookCoverUrl audiobook
        , "audiobook-cover-title" ~> audiobookTitle audiobook
        , "audiobook-description" ~> audiobookDescription audiobook
        , "episodes" ~> audiobookEpisodes audiobook
        ]

instance ToMustache Episode where
    toMustache episode = object
        [ "episode-title" ~> episodeTitle episode
        , "episode-url" ~> episodeUrl episode
        , "episode-track-url" ~> episodeTrackUrl episode
        , "episode-duration" ~> episodeDuration episode
        ]

instance ToMustache Audiobook where
    toMustache = object . toPairList

instance ToMustache Podcast where
    toMustache podcast = object $ [ 
        "base-url" ~> baseUrl podcast,
        "pub-day" ~> show (pubDay podcast)
        ] ++ (toPairList (audiobook podcast))

generatePodcast :: Day -> String -> Audiobook -> Podcast
generatePodcast day url audiobook = Podcast audiobook url day

generatePodcastFileName :: Podcast -> String
generatePodcastFileName (Podcast audiobook _ _) = title'' ++ ".xml"
    where
        replace :: Eq a => a -> a -> [a] -> [a]
        replace a b = map $ \c -> if c == a then b else c
        title = audiobookTitle audiobook
        title' = map toLower title
        title'' = replace ' ' '-' title'
