-----------------------------------------------------------------------------
--
-- Module      :  Sound.MusicBrainz.DiscId
-- Copyright   :  2013 André Twupack
-- License     :  GNU LESSER GENERAL PUBLIC LICENSE Version 3
--
-- This library is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this library.  If not, see <http://www.gnu.org/licenses/>.

-- Maintainer  :  André Twupack
-- Stability   :
-- Portability :
--
-- |
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.MusicBrainz.DiscId (
    getVersionString,
    getDefaultDevice,
    readFromDefaultCd,
    readFromCd,
    hasFeature,
    getFeatureList,
    DiscIdFeature(..),
    DiscId(..),
    TOC(..),
    Track(..)
) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Data.Map (Map, fromList)
import Control.Monad (mapM)
import Control.Applicative ((<$>))
import qualified Data.Vector.Storable.Mutable as M
import System.IO.Unsafe (unsafePerformIO)

-- | The information for a disc
data DiscId = DiscId {
    -- | MusicBrainz DiscID
    mbId :: String,
    -- | FreeDB DiscID
    freedbId :: String,
    -- | URL for submitting the DiscID to MusicBrainz
    submissionUrl :: String,
    -- | URL for retrieving CD information from MusicBrainz' web service
    webserviceUrl :: String,
    -- | The length of the disc in sectors
    sectors :: Int,
    -- | The TOC of the disc
    toc :: TOC,
    -- | The Media Catalogue Number for the disc
    mcn :: String} deriving Show

-- | The TOC of a disc
data TOC = TOC {
    -- | The number of the first track on this disc
    firstTrackNum :: Int,
    -- | The number of the last track on this disc
    lastTrackNum :: Int,
    -- | The tracks on the disc
    tracks :: Map Int Track} deriving Show

-- | The information for a track
data Track = Track {
    -- | The track number
    num :: Int,
    -- | The sector offset of a track
    offset :: Int,
    -- | The length of a track in sectors
    length :: Int,
    -- | The ISRC for a track
    isrc :: String} deriving Show

type DiscIdHandle = Ptr ()

foreign import ccall unsafe discid_get_version_string :: IO CString
foreign import ccall unsafe discid_get_default_device :: IO CString
foreign import ccall unsafe discid_new :: IO DiscIdHandle
foreign import ccall unsafe discid_free :: DiscIdHandle -> IO ()
foreign import ccall unsafe discid_get_error_msg :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_get_freedb_id :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_get_id :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_read :: DiscIdHandle -> CString -> IO CInt
foreign import ccall unsafe discid_get_first_track_num :: DiscIdHandle -> IO CInt
foreign import ccall unsafe discid_get_last_track_num :: DiscIdHandle -> IO CInt
foreign import ccall unsafe discid_get_submission_url :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_get_webservice_url :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_get_sectors :: DiscIdHandle -> IO CInt
foreign import ccall unsafe discid_get_track_length :: DiscIdHandle -> CInt -> IO CInt
foreign import ccall unsafe discid_get_track_offset :: DiscIdHandle -> CInt -> IO CInt
foreign import ccall unsafe discid_get_mcn :: DiscIdHandle -> IO CString
foreign import ccall unsafe discid_get_track_isrc :: DiscIdHandle -> CInt -> IO CString
foreign import ccall unsafe discid_has_feature :: CInt -> CInt
foreign import ccall unsafe discid_get_feature_list :: Ptr CString -> IO ()

-- | Features procided by libdiscid
data DiscIdFeature
    -- | Read disc TOC and calculate IDs
    = Read
    -- | Get Media Catalogue Number
    | MCN
    -- | Get ISRC
    | ISRC
    -- | Unknown feature
    | Unknown deriving Show

-- | A list of features supported by the current platform
getFeatureList :: [DiscIdFeature]
getFeatureList = unsafePerformIO $ do
    arr <- M.new 32
    M.unsafeWith arr discid_get_feature_list
    cp <- mapM (M.read arr) [0..31]
    fs <- mapM peekCString (takeWhile (/=nullPtr) cp)
    return $ toFeature <$> fs

toFeature :: String -> DiscIdFeature
toFeature s
    | s == "read" = Read
    | s == "isrc" = ISRC
    | s == "mcn" = MCN
    | otherwise = Unknown

toBool :: CInt -> Bool
toBool ib
    | ib == 0 = False
    | otherwise = True

-- | Check if a certain feature is implemented on the current platform
hasFeature :: DiscIdFeature -> Bool
hasFeature Read =  toBool $ discid_has_feature 1
hasFeature MCN =  toBool $ discid_has_feature 2
hasFeature ISRC =  toBool $ discid_has_feature 4

-- | Return the full version string of this library, including the name.
getVersionString :: String -- ^ a string containing the version of libdiscid.
getVersionString = unsafePerformIO $ discid_get_version_string >>= peekCString

-- | Return the name of the default disc drive for this operating system.
getDefaultDevice :: String -- ^ a string containing an operating system dependent device identifier
getDefaultDevice = unsafePerformIO $ discid_get_default_device >>= peekCString

-- | Read the disc in the default CD-ROM/DVD-ROM drive.
readFromDefaultCd :: IO (Either String DiscId)
readFromDefaultCd = readFromCd getDefaultDevice

-- | Read the disc in the given CD-ROM/DVD-ROM drive.
readFromCd :: String -> IO (Either String DiscId)
readFromCd dev = do
    handle <- discid_new
    result <- withCString dev $ \devC -> discid_read handle devC
    discId <- createDiscId handle result
    discid_free handle
    return discId

createDiscId :: DiscIdHandle -> CInt -> IO (Either String DiscId)
createDiscId handle result
    | result == 0 = do
        error <- discid_get_error_msg handle >>= peekCString
        return $ Left error
    | otherwise = do
        mbId <- discid_get_id handle >>= peekCString
        freedbId <- discid_get_freedb_id handle >>= peekCString
        submUrl <- discid_get_submission_url handle >>= peekCString
        websUrl <- discid_get_webservice_url handle >>= peekCString
        sectors <- fromIntegral <$> discid_get_sectors handle
        toc <- createTOC handle
        mcn <- discid_get_mcn handle >>= peekCString
        return $ Right (DiscId mbId freedbId submUrl websUrl sectors toc mcn)


createTrack :: DiscIdHandle -> CInt -> IO (Int, Track)
createTrack handle num = do
    length <- fromIntegral <$> discid_get_track_length handle num
    offset <- fromIntegral <$> discid_get_track_offset handle num
    isrc <- discid_get_track_isrc handle num >>= peekCString
    return (fromIntegral num, Track (fromIntegral num) offset length isrc)

createTOC :: DiscIdHandle -> IO TOC
createTOC handle = do
    first <- discid_get_first_track_num handle
    last <- discid_get_last_track_num handle
    tracks <- mapM (createTrack handle) [first..last]
    return $ TOC (fromIntegral first) (fromIntegral last) (fromList tracks)
