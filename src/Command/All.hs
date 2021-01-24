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
import Command.Single ( singleWithAuthor )
import Scraper.Playlist
    ( playlistPageNumbersScraper, playlistInfosScraper )

baseUrl = "https://www.raiplayradio.it"
playlistBaseUrl = "https://www.raiplayradio.it/programmi/adaltavoce/archivio/audiolibri/tutte/"

scrapeAudiobooksUrl :: IO (Maybe [(URL, String)])
scrapeAudiobooksUrl = do
    pageNumbers <- scrapeURL playlistBaseUrl playlistPageNumbersScraper
    case scrapePlaylistPages pageNumbers of
       Nothing -> return Nothing
       Just urls -> Just <$> urls

scrapePlaylistPages :: Maybe [String] -> Maybe (IO [(URL, String)])
scrapePlaylistPages pageNumbers = do
    pageNumbers' <- pageNumbers
    let playlistUrls = map (playlistBaseUrl ++) pageNumbers'
        audiobookInfos = mapM (`scrapeURL` playlistInfosScraper) playlistUrls
        flatAudiobookInfos = join . catMaybes <$> audiobookInfos
    return $ map (\(u, a) -> (concatBaseUrl u, a)) <$> flatAudiobookInfos
    where
        concatBaseUrl :: URL -> URL
        concatBaseUrl = (++) baseUrl

generateAll :: String -> IO ()
generateAll outdir = do
    infos <- scrapeAudiobooksUrl
    case infos of
        Nothing -> putStrLn "Error"
        Just infos' -> do
            mapM_ (\(url, author) -> singleWithAuthor url outdir author) infos'
            putStrLn "All done.\nEnjoy your books!"
