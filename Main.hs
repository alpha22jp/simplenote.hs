{-# LANGUAGE OverloadedStrings #-}

import Simplenote
import Control.Monad
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
  
showAllNotes :: [Note] -> String -> String -> IO ()
showAllNotes notes email token =
  forM_ notes (\i -> do note <- getNote email token (fromJust $ key i)
                        showNote note)

main :: IO ()
main = do
  let email = "abc@example.com"
  let password = "password"
  token <- getToken email password
  notes <- getIndex email token
  showAllNotes notes email token
  --
  -- note <- getNote email token "3c4b6b1c176111e581fcc9023998867e"
  -- let Just str = content note
  -- now <- getPOSIXTime
  -- let note' = note { content = Just (str ++ "Hello, world !!!\n"),
  --                    modifydate = Just (posixTimeToStr now) }
  -- updateNote email token note'
  --
  -- note <- createNote email token "New note 6\nTest for SimplenoteHS"
  -- showNote note
  return ()
