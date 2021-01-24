{-|
Module      : Command.All
Description : Generate podcast for all audiobooks
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module exposes the command that generates podcast feeds for all the 
audiobooks in Ad Alta Voce library.
-}

module Command.All(generateAll) where

import Control.Monad ( join )
import Data.Maybe ( catMaybes )
import Text.HTML.Scalpel ( scrapeURL, URL )
import Command.Single ( single )
import Scraper.Playlist
    ( playlistPageNumbersScraper, playlistsUrlScraper )

baseUrl = "https://www.raiplayradio.it"
playlistBaseUrl = "https://www.raiplayradio.it/programmi/adaltavoce/archivio/audiolibri/tutte/"

scrapeAudiobooksUrl :: IO (Maybe [URL])
scrapeAudiobooksUrl = do
    pageNumbers <- scrapeURL playlistBaseUrl playlistPageNumbersScraper
    case scrapePlaylistPages pageNumbers of
       Nothing -> return Nothing
       Just urls -> Just <$> urls

scrapePlaylistPages :: Maybe [String] -> Maybe (IO [URL])
scrapePlaylistPages pageNumbers = do
    pageNumbers' <- pageNumbers
    let playlistUrls = map (playlistBaseUrl ++) pageNumbers'
        audiobookUrls = mapM (`scrapeURL` playlistsUrlScraper) playlistUrls
        flatAudiobookUrls = join . catMaybes <$> audiobookUrls
    return $ map (baseUrl ++) <$> flatAudiobookUrls

generateAll :: String -> IO ()
generateAll outdir = do
    urls <- scrapeAudiobooksUrl
    case urls of
        Nothing -> putStrLn "Error"
        Just urls' -> do
            mapM_ (`single` outdir) urls'
            putStrLn "All done.\nEnjoy your books!"
