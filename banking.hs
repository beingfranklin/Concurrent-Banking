import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import System.Random

import qualified Data.Map as Map

type AccountName = Int
type Account = TVar Pound
type Pound = Integer
type Bank = TVar (Map.Map AccountName Account)

numberOfAccounts = 10
threads = 10
transactionsPerThread = 100
initialValue = 1000
minAmount = 10
maxAmount = 50

-- Get account by ID, create new empty account if it didn't exist
getAccount :: Bank -> AccountName -> STM Account
getAccount bank accountId = do
  accounts <- readTVar bank
  case Map.lookup accountId accounts of
    Just account -> return account
    Nothing -> do
      account <- newTVar initialValue
      writeTVar bank $ Map.insert accountId account accounts
      return account

-- Transfer amount between two accounts
transfer :: Pound -> Account -> Account -> STM ()
transfer amount from to = when (from /= to) $ do
  balanceFrom <- readTVar from
  balanceTo <- readTVar to
  --  Check so that accounts can't go negative
  when (balanceFrom - amount > 0) $ do
    -- putStrLn "Adding " ++ (balanceFrom - amount) ++ " from "++ balanceFrom ++ "to " ++ balanceTo
    writeTVar from $! balanceFrom - amount
    writeTVar to $! balanceTo + amount

randomTransaction :: Bank -> IO ()
randomTransaction bank = do
  -- Make a random transaction
  fromId <- randomRIO (1, numberOfAccounts)
  toId   <- randomRIO (1, numberOfAccounts)
  amount <- randomRIO (minAmount, maxAmount)

  -- Perform it atomically
  atomically $ do
    from <- getAccount bank fromId
    to   <- getAccount bank toId
    transfer amount from to

main = do
  bank <- newTVarIO Map.empty
  -- Starting  worker threads to do a number of random transactions
  workers <- replicateM threads $ do
    done <- newEmptyMVar
    forkIO $ do
      replicateM_ transactionsPerThread $ randomTransaction bank
      putMVar done ()
    return done

  -- Wait for worker threads to finish
  mapM_ takeMVar workers

  -- Print list of accounts and total bank balance
  putStrLn "----------------"
  summary <- atomically $ do
    accounts <- readTVar bank
    forM (Map.assocs accounts) $ \(accountId, account) -> do
      balance <- readTVar account
      return (accountId, balance)
  mapM_ print summary
  putStrLn "----------------"