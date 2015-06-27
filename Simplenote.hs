{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplenote where

import Network.HTTP.Conduit
import Network.HTTP (urlEncodeVars)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64.URL as Base64 (encode)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Data.Maybe

data Note = Note { key :: Maybe String,
                   content :: Maybe String,
                   modifydate :: Maybe String,
                   createdate :: Maybe String,
                   tags :: Maybe [String],
                   systemtags :: Maybe [String],
                   deleted :: Maybe Int,
                   version :: Maybe Int,
                   minversion :: Maybe Int,
                   syncnum :: Maybe Int
                 } deriving Show

nullNote :: Note
nullNote = Note Nothing Nothing Nothing Nothing Nothing
           Nothing Nothing Nothing Nothing Nothing 

deriveJSON defaultOptions { omitNothingFields = True } ''Note

strToPosixTime :: String -> POSIXTime
strToPosixTime str = realToFrac (read str :: Double)

posixTimeToStr :: POSIXTime -> String
posixTimeToStr = (formatTime defaultTimeLocale "%s%Q") . posixSecondsToUTCTime

baseUrl :: String
baseUrl = "https://app.simplenote.com/"

getToken :: String -> String -> IO String
getToken email pass = do
  req0 <- parseUrl $ baseUrl ++ "api/login"
  let req = req0 {
        method = "POST",
        requestBody = RequestBodyBS . Base64.encode . BS.pack $
                      urlEncodeVars [("email", email), ("password", pass)] }
  res <- withManager $ httpLbs req
  return $ (LBS.unpack . responseBody) res

data NoteIndex = NoteIndex { ncount :: Int,
                             ndata :: [Note],
                             nmark :: Maybe String
                           } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''NoteIndex

getIndex' :: String -> String -> Maybe String -> [Note] -> IO [Note]
getIndex' email token mark data0 = do
  req0 <- parseUrl $ baseUrl ++ "api2/index"
  let params0 = [("email", email), ("auth", token), ("length", "100")]
  let params = maybe params0 (\x -> ("mark", x) : params0) mark
  let req = req0 { queryString = BS.pack $ urlEncodeVars params }
  res <- withManager $ httpLbs req
  let index = fromJust . decode . responseBody $ res :: NoteIndex
  case nmark index of
    Nothing -> return $ data0 ++ ndata index
    x -> getIndex' email token x (data0 ++ ndata index)

getIndex :: String -> String -> IO [Note]
getIndex email token = getIndex' email token Nothing []

getNote :: String -> String -> String -> IO Note
getNote email token nkey = do
  req0 <- parseUrl $ baseUrl ++ "api2/data/" ++ nkey
  let params = [("email", email), ("auth", token)]
  let req = req0 { queryString = BS.pack $ urlEncodeVars params }
  res <- withManager $ httpLbs req
  let note = fromJust . decode . responseBody $ res :: Note
  return note

updateNote :: String -> String -> Note -> IO Note
updateNote email token note = do
  req0 <- parseUrl $ baseUrl ++ "api2/data" ++ maybe "" ('/':) (key note)
  let params = [("email", email), ("auth", token)]
  let req = req0 { method = "POST",
                   queryString = BS.pack $ urlEncodeVars params,
                   requestBody = RequestBodyLBS (encode note) }
  res <- withManager $ httpLbs req
  let note' = fromJust . decode . responseBody $ res :: Note
  return note' { content = content note }

createNote :: String -> String -> String -> IO Note
createNote email token str = do
  time <- getPOSIXTime >>= return . posixTimeToStr
  let note = nullNote { createdate = Just time, modifydate = Just time,
                        content = Just str }
  updateNote email token note

deleteNote :: String -> String -> String -> IO ()
deleteNote email token nKey = do
  req0 <- parseUrl $ baseUrl ++ "api2/data/" ++ nKey
  let params = [("email", email), ("auth", token)]
  let req = req0 { method = "DELETE",
                   queryString = BS.pack $ urlEncodeVars params }
  _ <- withManager $ httpLbs req
  return ()
