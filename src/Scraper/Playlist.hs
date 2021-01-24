{-|
Module      : Scraper.Playlist
Description : Scrape audiobook link from Rai Play Radio
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module contains all selectors and scrapers needed to retrieve the 
links to the audiobooks.
An example of a web page that can be scraped is available at the following 
<https://www.raiplayradio.it/programmi/adaltavoce/archivio/audiolibri/tutte link>
-}

{-# LANGUAGE OverloadedStrings #-}

module Scraper.Playlist
    ( playlistPageNumbersScraper
    , playlistInfosScraper
    ) where

import Text.HTML.Scalpel

playlistSelector :: Selector 
playlistSelector = "div" @: [hasClass "bloccoPlaylist"]

playlistUrlScraper :: Scraper String String
playlistUrlScraper = attr "href" "a"

playlistAuthorSelector :: Selector 
playlistAuthorSelector = "span" @: [hasClass "canale"]

playlistAuthorScraper :: Scraper String String 
playlistAuthorScraper = text playlistAuthorSelector

playlistInfoScraper :: Scraper String (String, String)
playlistInfoScraper = do
    url <- playlistUrlScraper
    author <- playlistAuthorScraper
    return (url, author)

-- |The 'playlistUrlScraper' function defines the scraper that retrieves all 
-- audiobooks url and author cointains in the playlist page.
playlistInfosScraper :: Scraper String [(String, String)]
playlistInfosScraper = chroots playlistSelector playlistInfoScraper

playlistPageNumberSelector :: Selector 
playlistPageNumberSelector = "ul" @: [hasClass "pagination"]
                             // ("li" @: [notP (hasClass "archivePaginatation")])

playlistPageNumberScraper :: Scraper String String 
playlistPageNumberScraper = text "a"

-- |Audiobooks are listed on multiple pages, 'playlistPageNumbersScraper' 
-- defines the scraper that retrieves the identifier of all pages that 
-- contain some audiobooks.
playlistPageNumbersScraper :: Scraper String [String ]
playlistPageNumbersScraper = chroots playlistPageNumberSelector playlistPageNumberScraper
