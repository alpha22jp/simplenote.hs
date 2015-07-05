{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplenote where

import Network.HTTP.Conduit
import Network.HTTP (urlEncode, urlEncodeVars)
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base64.URL as Base64 (encode)
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Control.Monad.Error
import Control.Monad.Trans.Resource

type SimplenoteManager = (Manager, String, String)

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
posixTimeToStr = formatTime defaultTimeLocale "%s%Q" . posixSecondsToUTCTime

baseUrl :: String
baseUrl = "https://app.simplenote.com/"

defaultRequest url method' params = do
  req0 <- parseUrl $ baseUrl ++ url
  return req0 {
    method = method',
    queryString = BS.pack $ urlEncodeVars params,
    checkStatus = \_ _ _ -> Nothing }

checkStatusCode :: Response body -> String
                -> (Response body -> IO (Either String a))
                -> IO (Either String a)
checkStatusCode res errMsg process =
  if code /= 200 then return . Left $ errMsg ++ show code else process res
  where code = statusCode . responseStatus $ res

getToken :: Manager -> String -> String -> IO (Either String String)
getToken mgr email pass = do
  req0 <- defaultRequest "api/login" "POST" []
  let req = req0 {
        requestBody = RequestBodyBS . Base64.encode . BS.pack $
                      urlEncodeVars [("email", email), ("password", pass)] }
  res <- httpLbs req mgr
  checkStatusCode res "Get token status error: " $
    return . Right . LBS.unpack . responseBody

data NoteIndex = NoteIndex { ncount :: Int,
                             ndata :: [Note],
                             nmark :: Maybe String
                           } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''NoteIndex

getIndex :: SimplenoteManager -> IO (Either String [Note])
getIndex snmgr = getIndex' Nothing [] where
  getIndex' mark data0 = do
    let (mgr, email, token) = snmgr
    let params0 = [("email", email), ("auth", token), ("length", "100")]
    let params = maybe params0 (\x -> ("mark", x) : params0) mark
    req <- defaultRequest "api2/index" "GET" params
    res <- httpLbs req mgr
    checkStatusCode res "Get index status error: "
      (\r -> case decode . responseBody $ r :: Maybe NoteIndex of
         Nothing -> return . Left $ "Get index JSON decode error"
         Just index -> case nmark index of
           Nothing -> return . Right $ data0 ++ ndata index
           x -> getIndex' x (data0 ++ ndata index))

getNote :: SimplenoteManager -> String -> IO (Either String Note)
getNote snmgr nkey = do
  let (mgr, email, token) = snmgr
  let params = [("email", email), ("auth", token)]
  req <- defaultRequest ("api2/data/" ++ nkey) "GET" params
  res <- httpLbs req mgr
  checkStatusCode res "Get note status error: "
    (\r -> case decode . responseBody $ r :: Maybe Note of
       Nothing -> return . Left $ "Get note JSON decode error"
       Just note -> return . Right $ note)

updateNote :: SimplenoteManager -> Note -> IO (Either String Note)
updateNote snmgr note = do
  let (mgr, email, token) = snmgr
  let params = [("email", email), ("auth", token)]
  req <- defaultRequest ("api2/data" ++ maybe "" ('/':) (key note)) "POST" params
  let note1 = note { content = fmap urlEncode (content note) }
  res <- httpLbs req { requestBody = RequestBodyLBS (encode note1) } mgr
  checkStatusCode res "Update note status error: "
    (\r -> case decode . responseBody $ r :: Maybe Note of
       Nothing -> return . Left $ "Update note JSON decode error"
       Just note' -> return . Right $ note' { content = content note })

createNote :: SimplenoteManager -> String -> IO (Either String Note)
createNote snmgr str = do
  time <- liftIO $ fmap posixTimeToStr getPOSIXTime
  let note = nullNote { createdate = Just time, modifydate = Just time,
                        content = Just str }
  updateNote snmgr note

deleteNote :: SimplenoteManager -> String -> IO (Either String ())
deleteNote snmgr nKey = do
  let (mgr, email, token) = snmgr
  let params = [("email", email), ("auth", token)]
  req <- defaultRequest ("api2/data/" ++ nKey) "DELETE" params
  res <- httpLbs req mgr
  let code = (statusCode . responseStatus) res
  if code /= 200
    then return . Left $ "Update note status error: " ++ show code
    else return . Right $ ()

newSimplenote :: String -> String -> IO (Either String SimplenoteManager)
newSimplenote email pass = do
  mgr <- newManager conduitManagerSettings
  ret <- getToken mgr email pass
  case ret of
    Right token -> return . Right $ (mgr, email, token)
    Left err -> return . Left $ err

closeSimplenote :: SimplenoteManager -> IO ()
closeSimplenote snmgr = do
  let (mgr, _, _) = snmgr
  closeManager mgr

runSimplenote :: String -> String
                 -> (SimplenoteManager -> IO (Either String a))
                 -> IO (Either String a)
runSimplenote email pass process = runResourceT $ do
  ret <- liftIO $ newSimplenote email pass
  case ret of
    Right snmgr -> liftIO $ process snmgr
    Left err -> return . Left $ err
