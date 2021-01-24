{-|
Module      : Scraper.Audiobook
Description : Scrape audiobook information from Rai Play Radio
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module contains all selectors and scrapers needed to retrieve the 
information from the audiobook.
An example of a web page that can be scraped is available at the following 
<https://www.raiplayradio.it/playlist/2017/12/Arancia-Meccanica-9d191ecb-23df-43fe-8e97-e423707f7de3.html link>
-}

{-# LANGUAGE OverloadedStrings #-}

module Scraper.Audiobook(audiobookScraper) where

import Text.HTML.Scalpel
import Types ( Audiobook(Audiobook), Episode(Episode) )

audiobookHeaderSelector :: Selector 
audiobookHeaderSelector = "div" @: [hasClass "descriptionProgramma"]

audiobookTitleScraper :: Scraper String String
audiobookTitleScraper = text 
                 $ audiobookHeaderSelector
                 // "h2"

audiobookDescriptionScraper :: Scraper String String
audiobookDescriptionScraper = text 
                       $ audiobookHeaderSelector 
                       // "span" @: [hasClass "textDescriptionProgramma"]

audiobookCoverUrlScraper :: Scraper String String
audiobookCoverUrlScraper = attr "src" 
                    $ "img" @: [hasClass "imgHomeProgramma"]

episodesSelector :: Selector 
episodesSelector = "ol" @: [hasClass "elencoPlaylist"] 
                   // "li"

episodeTitleScraper :: Scraper String String
episodeTitleScraper = text "h2"

episodeTrackUrlScraper :: Scraper String String
episodeTrackUrlScraper = attr "data-mediapolis" "li"

episodeCoverUrlScraper :: Scraper String String
episodeCoverUrlScraper = attr "data-image" "li"

episodeUrlScraper :: Scraper String String
episodeUrlScraper = attr "data-href" "li"

episodeDurationScraper :: Scraper String String
episodeDurationScraper = text 
                  $ "span" @: [hasClass "timePlaylist"]

episodeScraper :: Scraper String Episode
episodeScraper = do
    url <- episodeUrlScraper
    title <- episodeTitleScraper
    trackUrl <- episodeTrackUrlScraper
    episodeCover <- episodeCoverUrlScraper
    duration <- episodeDurationScraper
    return $ Episode url title trackUrl episodeCover duration

episodesListScraper :: Scraper String [Episode]
episodesListScraper = chroots episodesSelector episodeScraper

-- |The 'audiobookScraper' function defines the scraper that retrive the 
-- 'Audiobook' from the given web page.
audiobookScraper :: Scraper String Audiobook
audiobookScraper = do
    title <- audiobookTitleScraper
    description <- audiobookDescriptionScraper
    coverUrl <- audiobookCoverUrlScraper
    episodes <- episodesListScraper
    return $ Audiobook title description coverUrl episodes
