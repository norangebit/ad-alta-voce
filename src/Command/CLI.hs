{-|
Module      : Command.CLI
Description : Define CLI interface
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it

This module exposes the CLI of the program.
-}

module Command.CLI where

import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Types ( ParserInfo, Parser )
import Command.All ( generateAll )
import Command.Single ( single )

newtype AllOption = AllOption { outputDirectoryAll :: String}

data SingleOption = SingleOption { audiobookUrl :: String
                                 , outputDirectorySingle :: String }

data Command = All AllOption| Single SingleOption

singleParser :: Parser SingleOption
singleParser = SingleOption 
               <$> argument str (metavar "URL"
                    <> help "Audiobook url")
               <*> strOption (long "output" 
                    <> short 'o'
                    <> metavar "DIRECTORY"
                    <> help "Directory where save the podcast"
                    <> value "out"
                    <> showDefault)

allParser :: Parser AllOption
allParser = AllOption
            <$> strOption (long "output" 
                <> short 'o'
                <> metavar "DIRECTORY"
                <> help "Directory where save the podcasts"
                <> value "out"
                <> showDefault)

singleParserInfo :: ParserInfo Command
singleParserInfo = Single 
                   <$> info (singleParser <**> helper) 
                   (progDesc "Generate podcast of the given Ad Alta Voce url")

allParserInfo :: ParserInfo Command
allParserInfo = All 
                <$> info (allParser <**> helper)
                (progDesc "Generate podcast for all Ad Alta Voce audioboks")

commandParser :: Parser Command
commandParser = subparser (
                    command "single" singleParserInfo 
                    <> command "all" allParserInfo)

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper) 
                    (progDesc "Generate podcast of Ad Alta Voce audiobook")

execute :: Command -> IO ()
execute (Single opt) = single (audiobookUrl opt) (outputDirectorySingle opt)
execute (All opt)      = generateAll $ outputDirectoryAll opt
