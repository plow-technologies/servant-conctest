{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | @Servant.ConcTest@ is a package for testing that Servant servers are safe
-- and reliable under concurrent client access.
--
-- In particular, one can test *linearizability*, i.e. that the results produced
-- by the server under a concurrent execution are also possible under some
-- equivalent sequential one.
module Servant.ConcTest
  ( isLinearizable,
    withServantServer,
    Method (..),
  )
where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forM, forM_)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Network.HTTP.Client (Manager)
import Network.Wai.Handler.Warp (defaultSettings, withApplicationSettings)
import Servant
  ( Context (EmptyContext),
    Handler,
    HasServer (ServerT),
    Proxy,
    serveWithContext,
  )
import Servant.Client (BaseUrl (..), ClientM, Scheme (Http), mkClientEnv, runClientM)
import Servant.ConcTest.Utils (gather, interleavings)
import System.Exit (exitFailure)

-- | A client API function that stringifies its results, along with a name for debugging.
data Method = Method {name :: String, clientFn :: ClientM String}

instance Show Method where
  show (Method {name}) = name

-- | Checks if every concurrent execution produces the same results as some equivalent
-- sequential execution.
-- @reset@ is an API function to reset the state of the server in between tests.
-- @exec@ is a concurrent execution, specified as a list of API calls for each
-- thread, thus a [[Method]]
isLinearizable :: Manager -> BaseUrl -> ClientM () -> [[Method]] -> IO Bool
isLinearizable manager burl reset exec = do
  let clientEnv = mkClientEnv manager burl

  -- Generate all sequential interleavings and compute expected results
  let seqExecs :: [[(Int, Method)]] =
        -- Add the thread ID `t` to each API call before interleaving:
        interleavings $ map (\(t, calls) -> zip (repeat t) calls) $ zip [0 ..] exec
  seqResults <- forM seqExecs $ \seqExec -> do
    -- Reset server state
    runClientM reset clientEnv >>= \case
      Left err -> do
        putStrLn $ "There was an error:\n" ++ show err
        exitFailure
      Right () -> do
        let runFnWithThreadID (t, f) = (t,) <$> clientFn f
        let seqClient = sequence $ map runFnWithThreadID seqExec
        runClientM seqClient (mkClientEnv manager burl) >>= \case
          Left err -> error $ "Error computing expected results:\n" ++ show err
          Right res -> return res
  -- Gather the results and stringify for easy comparison
  let stringifyResults results =
        intercalate "|" $ map (intercalate ",") results
  let expectedResults = map (stringifyResults . gather) seqResults
  print expectedResults

  -- Run concurrently and check results are as expected
  forM_ [1 .. 500 :: Int] $ \i -> do
    -- Reset server state
    runClientM reset clientEnv >>= \case
      Left err -> do
        putStrLn $ "There was an error:\n" ++ show err
        exitFailure
      Right () -> do
        -- Run test execution concurrently
        res <- forConcurrently exec $ \threadFns -> do
          runClientM (sequence $ map clientFn threadFns) clientEnv
        case partitionEithers res of
          ([], results) -> do
            let resStr = stringifyResults results
            if elem resStr expectedResults
              then pure ()
              else do
                print i
                putStrLn $ "ERROR: " ++ show expectedResults ++ " does not contain " ++ resStr
                exitFailure
          -- print $ (i, stringifyResults results)
          -- if length (stringifyResults results) > 1000 then print i else pure ()
          (errs, _) -> do
            print i
            putStrLn $ "There was an error:\n" ++ show errs
            exitFailure
  pure True

withServantServer :: HasServer api '[] => Proxy api -> IO (ServerT api Handler) -> (BaseUrl -> IO a) -> IO a
withServantServer api server fn = do
  -- NOTE: this might avoid the socket not found error, but is MUCH slower:
  -- let port = 54321
  -- srv <- server
  -- withAsync (run port (serveWithContext api EmptyContext srv)) $ \_ -> do
  --   fn $ BaseUrl Http "localhost" port ""
  withApplicationSettings defaultSettings (return . serveWithContext api EmptyContext =<< server) $ \port ->
    fn $ BaseUrl Http "localhost" port ""
