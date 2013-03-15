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

import Sound.MusicBrainz.DiscId
import Text.Show.Pretty


main = do
    getFeatureList >>= print
    print $ hasFeature Read
    print $ hasFeature MCN
    print $ hasFeature ISRC
    version <- getVersionString
    putStrLn version
    dev <- getDefaultDevice
    putStrLn dev
    discId <- readFromDefaultCd
    putStrLn $ ppShow discId

