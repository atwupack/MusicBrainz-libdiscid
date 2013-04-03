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
    print getFeatureList
    print $ hasFeature Read
    print $ hasFeature MCN
    print $ hasFeature ISRC
    print getVersionString
    print getDefaultDevice
    discId <- readFromDefaultCd
    putStrLn $ ppShow discId

