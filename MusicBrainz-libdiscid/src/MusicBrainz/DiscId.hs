-----------------------------------------------------------------------------
--
-- Module      :  MusicBrainz.DiscId
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

{-# LANGUAGE ForeignFunctionInterface #-}

module MusicBrainz.DiscId (
    getVersionString,
    getDefaultDevice,
    readFromDefaultCd,
    readFromCd,
    DiscId(..),
    TOC(..),
    Track(..)
) where

import Foreign.C
import Foreign.Ptr (Ptr)
import Data.Map (Map, fromList)
import Control.Monad (mapM)

data DiscId = DiscId {  mbId :: String,
                        freedbId :: String,
                        submissionUrl :: String,
                        webserviceUrl :: String,
                        sectors :: Int,
                        toc :: TOC,
                        mcn :: String} deriving Show

data TOC = TOC {    firstTrackNum :: Int,
                    lastTrackNum :: Int,
                    tracks :: Map Int Track} deriving Show

data Track = Track {    num :: Int,
                        offset :: Int,
                        length :: Int,
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

getVersionString :: IO String
getVersionString = discid_get_version_string >>= peekCString

getDefaultDevice :: IO String
getDefaultDevice = discid_get_default_device >>= peekCString

readFromDefaultCd :: IO (Either String DiscId)
readFromDefaultCd = getDefaultDevice >>= readFromCd

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
        sectors <- discid_get_sectors handle
        toc <- createTOC handle
        mcn <- discid_get_mcn handle >>= peekCString
        return $ Right (DiscId mbId freedbId submUrl websUrl (fromIntegral sectors) toc mcn)


createTrack :: DiscIdHandle -> CInt -> IO (Int, Track)
createTrack handle num = do
    length <- discid_get_track_length handle num
    offset <- discid_get_track_offset handle num
    isrc <- discid_get_track_isrc handle num >>= peekCString
    return $ (fromIntegral num, Track (fromIntegral num) (fromIntegral offset) (fromIntegral length) isrc)

createTOC :: DiscIdHandle -> IO TOC
createTOC handle = do
    first <- discid_get_first_track_num handle
    last <- discid_get_last_track_num handle
    tracks <- mapM (\num -> createTrack handle num) [first..last]
    return $ TOC (fromIntegral first) (fromIntegral last) (fromList tracks)
