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
  createDate <- posixStrToLocalTimeStr $ fromJust (createdate note)
  modifyDate <- posixStrToLocalTimeStr $ fromJust (modifydate note)
  putStrLn $ "Create date: " ++ createDate
  putStrLn $ "Modify date: " ++ modifyDate
  putStrLn $ "Tags: " ++ show (tags note)
  putStrLn $ "System tags: " ++ show (systemtags note)
  putStrLn $ printf "Deleted: %d, Version: %d, Syncnum: %d\n"
    (fromJust $ deleted note) (fromJust $ version note) (fromJust $ syncnum note)
  putStrLn $ maybe "*** No content ***" id (content note)
  
main :: IO ()
main = do
  let email = "abc@example.com"
  let pass = "password"
  ret <- runSimplenote email pass $ \mgr -> do
    Right notes <- getIndex mgr
    forM_ notes (\i -> do ret <- getNote mgr (fromJust $ key i)
                          case ret of
                            Left err -> liftIO $ print err
                            Right note -> liftIO $ showNote note)
    -- ret <- liftIO $ createNote mgr "New note 1\nTest + / - _ for Simplenote.hs"
    -- case ret of
    --   Left err -> liftIO $ print err
    --   Right note -> liftIO $ showNote note
    return $ Right ()
  case ret of
    Left err -> print err
    Right _ -> return ()
