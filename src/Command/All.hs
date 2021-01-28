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
import Data.Text (unpack)
import Data.Maybe ( catMaybes )
import Text.Mustache
import Text.Parsec.Error ( ParseError )
import System.Directory ( createDirectoryIfMissing )
import System.IO
import Text.HTML.Scalpel ( scrapeURL, URL )
import Command.Single ( singleWithAuthor )
import Paths_ad_alta_voce ( getDataFileName )
import Scraper.Playlist
    ( playlistPageNumbersScraper, playlistInfosScraper )
import Types

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

writeIndex :: [Maybe Podcast] -> String -> String -> IO ()
writeIndex podcasts templateName outdir = do
    let index = Index $ catMaybes podcasts
    template <- compileIndexTemplate templateName
    writeIndexTemplate template index outputFile outdir
    where
        outputFile = outdir ++ "/" ++ take (length templateName - 9) templateName

compileIndexTemplate :: String -> IO (Either ParseError Template)
compileIndexTemplate templateName = do
  templateDir <- getDataFileName "templates"
  automaticCompile [".", templateDir] templateName

writeIndexTemplate :: Either ParseError Template -> Index -> String -> String -> IO ()
writeIndexTemplate (Left err) _ _ _ = print err
writeIndexTemplate (Right template) index fileName outdir = do
  createDirectoryIfMissing True outdir
  withFile fileName WriteMode  (\handle -> do
    hPutStr handle $ unpack indexContent
    putStrLn "Index done!\nAll done, enjoy your books!")
        where 
            indexContent = substitute template index

generateAll :: String -> Bool -> String -> IO ()
generateAll outdir indexMode indexTemplate = do
    infos <- scrapeAudiobooksUrl
    case infos of
        Nothing -> putStrLn "Error"
        Just infos' -> do
            podcasts <- mapM (\(url, author) -> singleWithAuthor url outdir author) infos'
            if indexMode
                then writeIndex podcasts indexTemplate outdir
                else putStrLn "All done.\nEnjoy your books!"
