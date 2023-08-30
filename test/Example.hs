{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Example (checkExample) where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client (ClientM, client)
import Servant.ConcTest (Method (..), isLinearizable, withServantServer)
import Test.QuickCheck (elements, forAll, ioProperty, quickCheck, vectorOf, verbose)

type API =
  "reset" :> Post '[JSON] ()
    :<|> "get" :> Get '[JSON] Int
    :<|> "put" :> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- 2 MVars, get reads both in turn without locks
-- put increments both in turn without locks
server :: IO (Server API)
server = do
  mvar1 <- MVar.newMVar 0
  mvar2 <- MVar.newMVar 0
  let getFn :: Handler Int
      getFn = do
        -- liftIO $ putStrLn "get"
        x1 <- liftIO $ MVar.readMVar mvar1
        x2 <- liftIO $ MVar.readMVar mvar2
        when (odd (x1 + x2)) $ do
          liftIO $ putStrLn "ERROR: not even"
        --   throwError err500 {errBody = "Not even"}
        return $ x1 + x2
  let putFn :: Handler ()
      putFn = do
        -- liftIO $ putStrLn "put"
        x1 <- liftIO $ MVar.takeMVar mvar1
        liftIO $ MVar.putMVar mvar1 (x1 + 1)
        x2 <- liftIO $ MVar.takeMVar mvar2
        liftIO $ MVar.putMVar mvar2 (x2 + 1)
        return ()
  let resetFn :: Handler ()
      resetFn = do
        -- liftIO $ putStrLn "reset"
        _ <- liftIO $ MVar.swapMVar mvar1 0
        _ <- liftIO $ MVar.swapMVar mvar2 0
        return ()
  return $ resetFn :<|> getFn :<|> putFn

resetClient :: ClientM ()
getClient :: ClientM Int
putClient :: ClientM ()
resetClient :<|> getClient :<|> putClient = client api

checkExample :: IO ()
checkExample = do
  manager <- newManager defaultManagerSettings

  -- Define the API to be tested
  let getClientFn = Method {name = "get", clientFn = show <$> getClient}
  let putClientFn = Method {name = "put", clientFn = show <$> putClient}
  let fns = [getClientFn, putClientFn]

  -- Generate concurrent executions to be tested
  let numThreads = 2
  let numCalls = 2
  let execGen = vectorOf numThreads $ vectorOf numCalls $ elements fns

  withServantServer api server $ \burl -> do
    quickCheck $
      verbose $
        forAll execGen (ioProperty . isLinearizable manager burl resetClient)
