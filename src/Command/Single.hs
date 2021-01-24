{-|
Module      : Command.Single
Description : Generate podcast for an audiobooks
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module exposes the command that generates podcast feed for an audiobooks 
in Ad Alta Voce library.
-}

module Command.Single(single) where

import Data.Text (unpack)
import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Text.HTML.Scalpel ( scrapeURL, URL )
import Text.Mustache
import Text.Parsec.Error ( ParseError )
import System.Directory ( createDirectoryIfMissing )
import System.IO
import Paths_ad_alta_voce ( getDataFileName )
import Scraper.Audiobook ( audiobookScraper )
import Types

compilePodcastTemplate :: IO (Either ParseError Template)
compilePodcastTemplate = do
  templateDir <- getDataFileName "templates"
  automaticCompile [templateDir] templateName
    where
      templateName = "podcast.mustache"

scrapeAudiobook :: URL -> IO (Maybe Audiobook)
scrapeAudiobook url = scrapeURL url audiobookScraper

writePodcastTemplate :: Either ParseError Template -> Maybe Podcast -> String -> IO ()
writePodcastTemplate (Left err) _ _ = print err
writePodcastTemplate _ Nothing _    = putStrLn "Error during audiobook parsing"
writePodcastTemplate (Right template) (Just podcast) outdir = do
  createDirectoryIfMissing True outdir
  withFile fileName WriteMode  (\handle -> do
    hPutStr handle $ unpack xmlPodcast
    putStrLn output)
        where 
            xmlPodcast = substitute template podcast
            title = audiobookTitle $ audiobook podcast
            fileName = outdir ++ "/" ++ generatePodcastFileName podcast
            output = title ++ " done!"

single :: String -> String -> IO ()
single url outdir = do
  day <- utctDay <$> getCurrentTime
  audiobook <- scrapeAudiobook url
  compiled <- compilePodcastTemplate

  let podcast = generatePodcast day url <$> audiobook

  writePodcastTemplate compiled podcast outdir
 
