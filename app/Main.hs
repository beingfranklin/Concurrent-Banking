module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import Lib
import System.Random

-- | Constants
threads = 10
transactionsPerThread = 100

-- | Main function
main :: IO ()
main = do
  customer <- newTVarIO Map.empty
  -- Starting worker threads to do a number of random transactions
  workers <- replicateM threads $ do
    done <- newEmptyMVar
    forkIO $ do
      replicateM_ transactionsPerThread $ randomTransaction customer
      putMVar done ()
    return done

  -- Wait for worker threads to finish
  mapM_ takeMVar workers

  -- Print list of accounts and total bank balance
  putStrLn "----------------"
  summary <- atomically $ do
    accounts <- readTVar customer
    forM (Map.assocs accounts) $ \(accountId, account) -> do
      balance <- readTVar account
      return (accountId, balance)
  mapM_ print summary
  putStrLn "----------------"