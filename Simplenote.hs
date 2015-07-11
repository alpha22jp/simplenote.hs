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
import Control.Monad.Trans.Error
import Control.Monad.Reader

type SimplenoteEnv = (Manager, String, String)
type SimplenoteManager a = ReaderT SimplenoteEnv (ErrorT String IO) a

data Note = Note { key :: Maybe String,
                   content :: Maybe String,
                   modifydate :: String,
                   createdate :: String,
                   tags :: [String],
                   systemtags :: [String],
                   deleted :: Int,
                   version :: Int,
                   minversion :: Int,
                   syncnum :: Int
                 } deriving Show

nullNote :: Note
nullNote = Note Nothing Nothing "" "" [] [] 0 0 0 0

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
                -> (Response body -> ErrorT String IO a)
                -> ErrorT String IO a
checkStatusCode res errMsg process =
  if code /= 200 then throwError $ errMsg ++ show code else process res
  where code = statusCode . responseStatus $ res

getToken :: Manager -> String -> String -> ErrorT String IO String
getToken mgr email pass = do
  req <- liftIO $ defaultRequest "api/login" "POST" []
  res <- httpLbs req {
    requestBody = RequestBodyBS . Base64.encode . BS.pack $
                  urlEncodeVars [("email", email), ("password", pass)] } mgr
  checkStatusCode res "Get token status error: " $
    return . LBS.unpack . responseBody

data NoteIndex = NoteIndex { ncount :: Int,
                             ndata :: [Note],
                             nmark :: Maybe String
                           } deriving Show

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''NoteIndex

getIndex' :: Maybe String -> [Note] -> SimplenoteManager [Note]
getIndex' mark notes = do
  env@(mgr, email, token) <- ask
  let params0 = [("email", email), ("auth", token), ("length", "100")]
  let params = maybe params0 (\x -> ("mark", x) : params0) mark
  req <- liftIO $ defaultRequest "api2/index" "GET" params
  res <- httpLbs req mgr
  lift $ checkStatusCode res "Get index status error: "
    (\r -> case decode . responseBody $ r :: Maybe NoteIndex of
       Nothing -> throwError "Get index JSON decode error"
       Just index -> case nmark index of
         Nothing -> return $ notes ++ ndata index
         mark' -> runReaderT (getIndex' mark' $ notes ++ ndata index) env)
  
getIndex :: SimplenoteManager [Note]
getIndex = getIndex' Nothing []

getNote :: String -> SimplenoteManager Note
getNote nkey = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data/" ++ nkey) "GET" params
  res <- httpLbs req mgr
  lift $ checkStatusCode res "Get note status error: "
    (\r -> case decode . responseBody $ r :: Maybe Note of
       Nothing -> throwError "Get note JSON decode error"
       Just note -> return note)

updateNote :: Note -> SimplenoteManager Note
updateNote note = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data" ++ maybe "" ('/':) (key note))
         "POST" params
  let note1 = note { content = fmap urlEncode (content note) }
  res <- httpLbs req { requestBody = RequestBodyLBS (encode note1) } mgr
  lift $ checkStatusCode res "Update note status error: "
    (\r -> case decode . responseBody $ r :: Maybe Note of
       Nothing -> throwError "Update note JSON decode error"
       Just note' -> return $ note' { content = content note })

createNote :: String -> SimplenoteManager Note
createNote str = do
  time <- liftIO $ fmap posixTimeToStr getPOSIXTime
  let note = nullNote { createdate = time, modifydate = time,
                        content = Just str }
  updateNote note

deleteNote :: String -> SimplenoteManager ()
deleteNote nKey = do
  (mgr, email, token) <- ask
  let params = [("email", email), ("auth", token)]
  req <- liftIO $ defaultRequest ("api2/data/" ++ nKey) "DELETE" params
  res <- httpLbs req mgr
  lift $ checkStatusCode res "Update note status error: " (\_ -> return ())

newSimplenote :: String -> String -> ErrorT String IO SimplenoteEnv
newSimplenote email pass = do
  mgr <- liftIO $ newManager conduitManagerSettings
  token <- getToken mgr email pass
  return (mgr, email, token)

closeSimplenote :: SimplenoteEnv -> IO ()
closeSimplenote env = do
  let (mgr, _, _) = env
  closeManager mgr

runSimplenote :: String -> String
                 -> SimplenoteManager ()
                 -> IO (Either String ())
runSimplenote email pass process = runResourceT $ do
  ret <- liftIO $ runErrorT $ newSimplenote email pass
  case ret of
    Left err -> return . Left $ err
    Right env -> liftIO $ runErrorT $ runReaderT process env
