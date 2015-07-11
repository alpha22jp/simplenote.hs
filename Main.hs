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
  ret <- runSimplenote email pass $ do
    notes <- getIndex
    forM_ notes (\i -> do note <- getNote (fromJust $ key i)
                          liftIO $ showNote note)
    note <- getNote "faab06d722bc11e5b1f671c420ef68f5"
    liftIO $ showNote note
    -- note <- createNote "New note 1\nTest + / - _ for Simplenote.hs"
    time <- liftIO $ fmap posixTimeToStr getPOSIXTime
    note' <- updateNote note {
      modifydate = time,
      content = fmap  (++ "Updated !!!\n") (content note) }
    liftIO $ showNote note'
  case ret of
    Left err -> print err
    _ -> return ()
