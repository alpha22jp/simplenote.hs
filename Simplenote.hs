{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplenote where

import Network.HTTP.Conduit
import Network.HTTP (urlEncodeVars)
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64.URL as Base64 (encode)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Data.Maybe
import Control.Monad.Error
import Control.Monad.Reader

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

getToken :: Manager -> String -> String -> (ErrorT String IO) String
getToken mgr email pass = do
  req0 <- parseUrl $ baseUrl ++ "api/login"
  let req = req0 {
        checkStatus = \_ _ _ -> Nothing,
        method = "POST",
        requestBody = RequestBodyBS . Base64.encode . BS.pack $
                      urlEncodeVars [("email", email), ("password", pass)] }
  res <- httpLbs req mgr
  case (statusCode . responseStatus) res of
    200 -> return $ (LBS.unpack . responseBody) res
    x -> throwError $ "Get token status error: " ++ show x

data NoteIndex = NoteIndex { ncount :: Int,
                             ndata :: [Note],
                             nmark :: Maybe String
                           } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''NoteIndex

getIndex' :: Maybe String -> [Note]
          -> ReaderT (Manager, String, String) (ErrorT String IO) [Note]
getIndex' mark data0 = do
  (mgr, email, token) <- ask
  req0 <- parseUrl $ baseUrl ++ "api2/index"
  let params0 = [("email", email), ("auth", token), ("length", "100")]
  let params = maybe params0 (\x -> ("mark", x) : params0) mark
  let req = req0 { queryString = BS.pack $ urlEncodeVars params }
  res <- httpLbs req mgr
  let index = fromJust . decode . responseBody $ res :: NoteIndex
  case nmark index of
    Nothing -> return $ data0 ++ ndata index
    x -> getIndex' x (data0 ++ ndata index)

getIndex :: ReaderT (Manager, String, String) (ErrorT String IO) [Note]
getIndex = getIndex' Nothing []

getNote :: String -> ReaderT (Manager, String, String) (ErrorT String IO) Note
getNote nkey = do
  (mgr, email, token) <- ask
  req0 <- parseUrl $ baseUrl ++ "api2/data/" ++ nkey
  let params = [("email", email), ("auth", token)]
  let req = req0 { queryString = BS.pack $ urlEncodeVars params }
  res <- httpLbs req mgr
  let note = fromJust . decode . responseBody $ res :: Note
  return note

updateNote :: Note -> ReaderT (Manager, String, String) (ErrorT String IO) Note
updateNote note = do
  (mgr, email, token) <- ask
  req0 <- parseUrl $ baseUrl ++ "api2/data" ++ maybe "" ('/':) (key note)
  let params = [("email", email), ("auth", token)]
  let req = req0 { method = "POST",
                   queryString = BS.pack $ urlEncodeVars params,
                   requestBody = RequestBodyLBS (encode note) }
  res <- httpLbs req mgr
  let note' = fromJust . decode . responseBody $ res :: Note
  return note' { content = content note }

createNote :: String -> ReaderT (Manager, String, String) (ErrorT String IO) Note
createNote str = do
  time <- liftIO $ getPOSIXTime >>= return . posixTimeToStr
  let note = nullNote { createdate = Just time, modifydate = Just time,
                        content = Just str }
  updateNote note

deleteNote :: String -> ReaderT (Manager, String, String) (ErrorT String IO) ()
deleteNote nKey = do
  (mgr, email, token) <- ask
  req0 <- parseUrl $ baseUrl ++ "api2/data/" ++ nKey
  let params = [("email", email), ("auth", token)]
  let req = req0 { method = "DELETE",
                   queryString = BS.pack $ urlEncodeVars params }
  _ <- httpLbs req mgr
  return ()

execSimplenote :: String -> String
               -> ReaderT (Manager, String, String) (ErrorT String IO) a
               -> IO (Either String ())
execSimplenote email pass process = runErrorT $ do
  mgr <- liftIO $ newManager conduitManagerSettings
  token <- getToken mgr email pass
  runReaderT process (mgr, email, token)
  liftIO $ closeManager mgr
  return ()
