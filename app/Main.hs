module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import Lib
import System.Random

-- | Constants used in Main
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
      -- replicateM_ is Like replicateM, but discards the result.
      replicateM_ transactionsPerThread $ randomTransaction customer
      putMVar done ()
    return done

  -- Wait for worker threads to finish
  -- mapM_ is Like mapM, but discards the result.
  mapM_ takeMVar workers

  -- Print list of accounts and total bank balance
  putStrLn "----------------"
  putStrLn "(accountNo, accountName, balance)"
  summary <- atomically $ do
    accounts <- readTVar customer
    forM (Map.assocs accounts) $ \(accountName, account) -> do
      balance <- readTVar account
      accountNo <- return accountName
      return (accountName, accountNo, balance)
  mapM_ print summary
  putStrLn "----------------"