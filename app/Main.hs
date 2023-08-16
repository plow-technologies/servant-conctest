{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (defaultSettings, withApplicationSettings)
import Servant
import Servant.Client (BaseUrl (..), ClientM, Scheme (Http), client, mkClientEnv, runClientM)
import System.Exit (exitFailure)
import Test.Hspec

type API = "reset" :> Post '[JSON] ()
  :<|> "get" :> Get '[JSON] Int
  :<|> "put" :> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- 2 MVars, get reads both in turn without locks
-- put increments both in turn without locks
server :: IO (Server API)
server = do
  mvar1 <- newMVar 0
  mvar2 <- newMVar 0
  let getFn :: Handler Int
      getFn = do
        -- liftIO $ putStrLn "get"
        x1 <- liftIO $ readMVar mvar1
        x2 <- liftIO $ readMVar mvar2
        when (odd (x1 + x2)) $ do
          liftIO $ putStrLn "ERROR: not even"
        --   throwError err500 {errBody = "Not even"}
        return $ x1 + x2
  let putFn :: Handler ()
      putFn = do
        -- liftIO $ putStrLn "put"
        x1 <- liftIO $ takeMVar mvar1
        liftIO $ putMVar mvar1 (x1 + 1)
        x2 <- liftIO $ takeMVar mvar2
        liftIO $ putMVar mvar2 (x2 + 1)
        return ()
  let resetFn :: Handler ()
      resetFn = do
        -- liftIO $ putStrLn "reset"
        _ <- liftIO $ swapMVar mvar1 0
        _ <- liftIO $ swapMVar mvar2 0
        return ()
  return $ resetFn :<|> getFn :<|> putFn

resetClient :: ClientM ()
getClient :: ClientM Int
putClient :: ClientM ()
resetClient :<|> getClient :<|> putClient = client api

-- | Generate all possible interleavings of the input lists.
-- From https://stackoverflow.com/a/41929156
-- NOTE: is there a more efficient way to do this?
interleavings :: [[a]] -> [[a]]
interleavings = go . filter (not . null)
  where
    go [] = [[]]
    go xss = do
      (xssl, x : xs, xssr) <- zippers xss
      (x :) <$> interleavings ([xs | not (null xs)] ++ xssl ++ xssr)
    zippers :: [a] -> [([a], a, [a])]
    zippers = go' []
      where
        go' l (h : r) = (l, h, r) : go' (h : l) r
        go' _ [] = []

-- | Convert a list of tuples to a list of lists, using the integers as indexes
-- in the returned list
-- TODO test that gather is the inverse of interleavings
gather :: [(Int, a)] -> [[a]]
gather [] = []
gather ((i, x) : xs) = insert i x (gather xs)

-- | Insert an element into the list at a given index
insert :: Int -> a -> [[a]] -> [[a]]
-- If the index is zero, prepend the element to the first sublist or create a new sublist if the list is empty
insert 0 x [] = [[x]]
insert 0 x (l : ls) = (x : l) : ls
-- If the index is negative, return the original list
insert i _ ls | i < 0 = ls
-- If the index is positive, recurse on the tail of the list and decrement the index
insert i x (l : ls) = l : insert (i - 1) x ls
-- If the index is larger than the length of the list, append the element to the last sublist or create a new sublist if the list is empty
insert i x [] = replicate i [] ++ [[x]]

withServantServer :: HasServer api '[] => Proxy api -> IO (ServerT api Handler) -> (BaseUrl -> IO a) -> IO a
withServantServer api server fn = do
  withApplicationSettings defaultSettings (return . serveWithContext api EmptyContext =<< server) $ \port ->
    fn $ BaseUrl Http "localhost" port ""

  -- NOTE: this might avoid the socket not found error, but is MUCH slower:
  -- let port = 54321
  -- srv <- server
  -- withAsync (run port (serveWithContext api EmptyContext srv)) $ \_ -> do
  --   fn $ BaseUrl Http "localhost" port ""

main :: IO ()
main = do
  -- TODO is it okay to share manager among threads?
  manager <- newManager defaultManagerSettings

  -- Define the concurrent execution to be tested
  let getClientFn = show <$> getClient
  let putClientFn = show <$> putClient
  let exec :: [[ClientM String]] = [[putClientFn, putClientFn], [getClientFn]]

  -- Generate all sequential interleavings and compute expected results
  let seqExecs :: [[(Int, ClientM String)]] =
        -- Add the thread ID to each invocation before interleaving:
        interleavings $ map (\(t, es) -> zip (repeat t) es) $ zip [0 ..] exec
  seqResults <- forM seqExecs $ \seqExec -> do
    -- TODO also use reset here to speed pre-computation if necessary
    withServantServer api server $ \burl -> do
      let runFnWithThreadID (t, f) = (t,) <$> f
      let seqClient = sequence $ map runFnWithThreadID seqExec
      runClientM seqClient (mkClientEnv manager burl) >>= \case
        Left err -> error $ "Error computing expected results:\n" ++ show err
        Right res -> return res
  -- Gather the results and stringify for easy comparison
  let stringifyResults results =
        intercalate "|" $ map (intercalate ",") results
  let expectedResults = map (stringifyResults . gather) seqResults
  print expectedResults -- TODO remove

  -- Run concurrently and check results are as expected
  withServantServer api server $ \burl -> do
    forM_ [1 .. 1000 :: Int] $ \i -> do
      let clientEnv = mkClientEnv manager burl
      -- Reset server state
      runClientM resetClient clientEnv >>= \case
        Left err -> do
          putStrLn $ "There was an error:\n" ++ show err
          exitFailure
        Right () -> do
          -- Run test execution concurrently
          res <- forConcurrently exec $ \threadFns -> do
            runClientM (sequence threadFns) clientEnv
          case partitionEithers res of
            ([], results) -> do
              shouldContain expectedResults [stringifyResults results]
              -- print $ (i, stringifyResults results)
              -- if length (stringifyResults results) > 1000 then print i else pure ()
            (errs, _) -> do
              print i
              putStrLn $ "There was an error:\n" ++ show errs
              exitFailure
      return ()
