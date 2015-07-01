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
  ret <- execSimplenote email pass $ do
    notes <-  getIndex
    forM_ notes (\i -> do note <- getNote (fromJust $ key i)
                          liftIO $ showNote note)
    note <- createNote "New note 2\nTest for SimplenoteHS"
    liftIO $ showNote note
  case ret of
    Left str -> print str
    Right _ -> return ()
