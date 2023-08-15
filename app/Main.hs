{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (defaultSettings, withApplicationSettings)
import Servant
  ( Context (EmptyContext),
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    Server,
    serveWithContext,
    type (:<|>) (..),
  )
import Servant.Client (BaseUrl (..), ClientM, Scheme (Http), client, mkClientEnv, runClientM)
import System.Exit (exitFailure)

type API = Get '[JSON] Int :<|> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- 2 MVars, get reads both in turn without locks
-- post increments both in turn without locks
server :: IO (Server API)
server = do
  mvar1 <- newMVar 0
  mvar2 <- newMVar 0
  let getFn :: Handler Int
      getFn = do
        x1 <- liftIO $ readMVar mvar1
        x2 <- liftIO $ readMVar mvar2
        when (odd (x1 + x2)) $ do
          liftIO $ putStrLn "ERROR: not even"
        --   throwError err500 {errBody = "Not even"}
        return $ x1 + x2
  let postFn :: Handler ()
      postFn = do
        x1 <- liftIO $ takeMVar mvar1
        liftIO $ putMVar mvar1 (x1 + 1)
        x2 <- liftIO $ takeMVar mvar2
        liftIO $ putMVar mvar2 (x2 + 1)
        return ()
  return $ getFn :<|> postFn

getClient :: ClientM Int
postClient :: ClientM ()
getClient :<|> postClient = client api

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  let getClientFn = show <$> getClient
  let postClientFn = show <$> postClient
  let exec :: [ClientM String] = [postClientFn, postClientFn, getClientFn]

  let stringifyResults results = intercalate "," results

  -- Run test 5000 times in order to make it more likely to see concurrency errors
  forM_ [1 .. 5000 :: Int] $ \i -> do
    -- Create the server anew each test because then we can compare the results of the client calls with an expected set
    withApplicationSettings defaultSettings (return . serveWithContext api EmptyContext =<< server) $ \port -> do
      let burl = BaseUrl Http "localhost" port ""
      -- For brevity I'm calling the API sequentially here, but the error occurs when I call it concurrently too
      res <- runClientM (sequence exec) $ mkClientEnv manager burl

      case res of
        Right results ->
          -- This is a dummy line to force evaluation, in reality we'd check that the results satisfy some test condition here:
          if length (stringifyResults results) > 1000 then print i else pure ()
        Left err -> do
          print i
          putStrLn $ "There was an error:\n" ++ show err
          exitFailure
      return ()
