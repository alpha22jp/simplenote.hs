{-# LANGUAGE ScopedTypeVariables #-}

module SafeAction where

import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Control.Exception

safeAction :: forall a. IO a -> ErrorT String IO a
safeAction action = do
  ret <- liftIO (try action :: IO (Either SomeException a))
  case ret of
    Left  e -> throwError $ show e
    Right r -> return r
