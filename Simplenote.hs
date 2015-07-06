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
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Reader

type SimplenoteEnv = (Manager, String, String)
type SimplenoteManager a = ReaderT SimplenoteEnv a

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

defaultRequest :: String -> BS.ByteString -> [(String, String)] -> IO Request
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
  req <- defaultRequest "api/login" "POST" []
  res <- httpLbs req {
    requestBody = RequestBodyBS . Base64.encode . BS.pack $
                  urlEncodeVars [("email", email), ("password", pass)] } mgr
  checkStatusCode res "Get token status error: " $
    return . Right . LBS.unpack . responseBody

data NoteIndex = NoteIndex { ncount :: Int,
                             ndata :: [Note],
                             nmark :: Maybe String
                           } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''NoteIndex

getIndex :: SimplenoteManager IO (Either String [Note])
getIndex = do
  (mgr, email, token) <- ask
  liftIO $ getIndex' (mgr, email, token) Nothing [] where
    getIndex' (mgr, email, token) mark data0 = do
      let params0 = [("email", email), ("auth", token), ("length", "100")]
      let params = maybe params0 (\x -> ("mark", x) : params0) mark
      req <- defaultRequest "api2/index" "GET" params
      res <- httpLbs req mgr
      checkStatusCode res "Get index status error: "
        (\r -> case decode . responseBody $ r :: Maybe NoteIndex of
           Nothing -> return . Left $ "Get index JSON decode error"
           Just index -> case nmark index of
             Nothing -> return . Right $ data0 ++ ndata index
             x -> getIndex' (mgr, email, token) x (data0 ++ ndata index))

getNote :: String -> SimplenoteManager IO (Either String Note)
getNote nkey = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data/" ++ nkey) "GET" params
  res <- liftIO $ httpLbs req mgr
  liftIO $ checkStatusCode res "Get note status error: "
    (\r -> liftIO . return $ case decode . responseBody $ r :: Maybe Note of
       Nothing -> Left $ "Get note JSON decode error"
       Just note -> Right $ note)

updateNote :: Note -> SimplenoteManager IO (Either String Note)
updateNote note = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data" ++ maybe "" ('/':) (key note))
         "POST" params
  let note1 = note { content = fmap urlEncode (content note) }
  res <- liftIO $ httpLbs req { requestBody = RequestBodyLBS (encode note1) } mgr
  liftIO $ checkStatusCode res "Update note status error: "
    (\r -> liftIO . return $ case decode . responseBody $ r :: Maybe Note of
       Nothing -> Left $ "Update note JSON decode error"
       Just note' -> Right $ note' { content = content note })

createNote :: String -> SimplenoteManager IO (Either String Note)
createNote str = do
  time <- liftIO $ fmap posixTimeToStr getPOSIXTime
  let note = nullNote { createdate = Just time, modifydate = Just time,
                        content = Just str }
  updateNote note

deleteNote :: String -> SimplenoteManager IO (Either String ())
deleteNote nKey = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data/" ++ nKey) "DELETE" params
  res <- liftIO $ httpLbs req mgr
  liftIO $ checkStatusCode res "Update note status error: "
    (\_ -> liftIO . return . Right $ ())

newSimplenote :: String -> String -> IO (Either String SimplenoteEnv)
newSimplenote email pass = do
  mgr <- newManager conduitManagerSettings
  ret <- getToken mgr email pass
  case ret of
    Right token -> return . Right $ (mgr, email, token)
    Left err -> return . Left $ err

closeSimplenote :: SimplenoteEnv -> IO ()
closeSimplenote env = do
  let (mgr, _, _) = env
  closeManager mgr

runSimplenote :: String -> String
                 -> SimplenoteManager IO (Either String a)
                 -> IO (Either String a)
runSimplenote email pass process = runResourceT $ do
  ret <- liftIO $ newSimplenote email pass
  case ret of
    Right env -> liftIO $ runReaderT process env
    Left err -> return . Left $ err
