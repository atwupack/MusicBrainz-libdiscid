{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import MusicBrainz.DiscId
import Text.Show.Pretty


main = do
    version <- getVersionString
    putStrLn version
    dev <- getDefaultDevice
    putStrLn dev
    discId <- readFromDefaultCd
    putStrLn $ ppShow discId

