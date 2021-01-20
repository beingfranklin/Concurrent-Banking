-- | Module header for module containing getAccount, transfer and randomTransaction methods
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

-- | Customer Datatype declaration. Its formed using the AccountName and Balance.
type AccountName = Int
type AccountNo = Int
type Pounds = Integer
type Balance = TVar Pounds
type Customer = TVar (Map.Map AccountName Balance)

-- | Constants used in Lib
numberOfAccounts = 10
minAmount = 10
maxAmount = 50
initialBankBalance = 1000

-- | Get account by ID, create new empty account if it didn't exist
getAccount :: Customer -> AccountName -> STM Balance
getAccount customer accountName = do
  accounts <- readTVar customer
  case Map.lookup accountName accounts of
    Just account -> return account
    Nothing -> do
      account <- newTVar initialBankBalance
      writeTVar customer $ Map.insert accountName account accounts
      return account

-- | Transfer amount between two accounts. The amounts are transferred between
transfer :: Pounds -> Balance -> Balance -> STM ()
transfer amount from to = when (from /= to) $ do
  balanceFrom <- readTVar from
  balanceTo <- readTVar to
  --  Check so that accounts can't go negative
  when (balanceFrom - amount > 0) $ do
    writeTVar from $! balanceFrom - amount
    writeTVar to $! balanceTo + amount

-- | Generate random transaction.Here we generate three random valuues - random accounts to debit and credit, and random sum to do the same. All the transactions are done atomically, either full gets executed or none.
randomTransaction :: Customer -> IO ()
randomTransaction customer = do
  -- Make a random transaction
  fromAccnNo <- randomRIO (1, numberOfAccounts)
  toAccnNo <- randomRIO (1, numberOfAccounts)
  amount <- randomRIO (minAmount, maxAmount)

  -- Perform the transfer amount atomically.
  atomically $ do
    from <- getAccount customer fromAccnNo
    to <- getAccount customer toAccnNo
    transfer amount from to
