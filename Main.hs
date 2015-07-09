{-# LANGUAGE OverloadedStrings #-}

import Simplenote
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Text.Printf
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale

posixStrToLocalTimeStr :: String -> IO String
posixStrToLocalTimeStr str = do
  time <- utcToLocalZonedTime . posixSecondsToUTCTime . strToPosixTime $ str
  return $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" time

showNote :: Note -> IO ()
showNote note = do
  putStrLn $ "=== " ++ fromJust (key note)
  createDate <- posixStrToLocalTimeStr (createdate note)
  modifyDate <- posixStrToLocalTimeStr (modifydate note)
  putStrLn $ "Create date: " ++ createDate
  putStrLn $ "Modify date: " ++ modifyDate
  putStrLn $ "Tags: " ++ show (tags note)
  putStrLn $ "System tags: " ++ show (systemtags note)
  putStrLn $ printf "Deleted: %d, Version: %d, Syncnum: %d\n"
    (deleted note) (version note) (syncnum note)
  putStrLn $ maybe "*** No content ***" id (content note)
  
main :: IO ()
main = do
  let email = "abc@example.com"
  let pass = "password"
  _ <- runSimplenote email pass $ do
    ret <- getIndex
    case ret of
      Left err -> liftIO $ print err
      Right notes -> 
        forM_ notes (\i -> do ret <- getNote (fromJust $ key i)
                              case ret of
                                Left err -> liftIO $ print err
                                Right note -> liftIO $ showNote note)
    ret <- getNote "faab06d722bc11e5b1f671c420ef68f5"
    -- ret <- createNote "New note 1\nTest + / - _ for Simplenote.hs"
    case ret of
      Left err -> liftIO $ print err
      Right note -> do
        liftIO $ showNote note
        time <- liftIO $ fmap posixTimeToStr getPOSIXTime
        Right note' <- updateNote note {
          modifydate = time,
          content = fmap  (++ "Updated !!!\n") (content note) }
        liftIO $ showNote note'
    return $ Right ()
  return ()
