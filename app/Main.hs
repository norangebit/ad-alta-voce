{-|
Module      : Main
Copyright   : (c) Raffaele Mignone 2021
License     : GPL-3
Maintainer  : git@norangeb.it
-}

module Main where

import Options.Applicative ( execParser )
import Command.CLI ( commandParserInfo, execute )

main :: IO ()
main = do 
  cliCommand <- execParser commandParserInfo
  execute cliCommand
  