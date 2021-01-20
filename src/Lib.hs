-- | Module header
module Lib
  ( getAccount,
    transfer,
    randomTransaction,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import System.Random

-- | Customer Datatype
type AccountName = Int

-- type AccountNo = Int
type Pound = Integer
type Balance = TVar Pound
type Customer = TVar (Map.Map AccountName Balance)

-- | Constants
numberOfAccounts = 10
minAmount = 10
maxAmount = 50
initialValue = 1000

-- | Get account by ID, create new empty account if it didn't exist
getAccount :: Customer -> AccountName -> STM Balance
getAccount customer accountId = do
  accounts <- readTVar customer
  case Map.lookup accountId accounts of
    Just account -> return account
    Nothing -> do
      account <- newTVar initialValue
      writeTVar customer $ Map.insert accountId account accounts
      return account

-- | Transfer amount between two accounts
transfer :: Pound -> Balance -> Balance -> STM ()
transfer amount from to = when (from /= to) $ do
  balanceFrom <- readTVar from
  balanceTo <- readTVar to
  --  Check so that accounts can't go negative
  when (balanceFrom - amount > 0) $ do
    -- putStrLn "Adding " ++ (balanceFrom - amount) ++ " from "++ balanceFrom ++ "to " ++ balanceTo
    writeTVar from $! balanceFrom - amount
    writeTVar to $! balanceTo + amount

-- | Generate random transaction
randomTransaction :: Customer -> IO ()
randomTransaction customer = do
  -- Make a random transaction
  fromId <- randomRIO (1, numberOfAccounts)
  toId <- randomRIO (1, numberOfAccounts)
  amount <- randomRIO (minAmount, maxAmount)

  -- Perform it atomically
  atomically $ do
    from <- getAccount customer fromId
    to <- getAccount customer toId
    transfer amount from to
