# Concurrent Banking

## Aim

To simulate a concurrent “banking” system. The main program should spawn ten “customer” threads, and each of these threads model a bank account with a starting balance of £1000. 
The customers should then (at random intervals) choose one of the other customers (at random) and transfer a random amount of money (between £10 and £50) into their account.

## Running the program

To compile the program just use the following command

 `stack ghci banking.hs -- -threaded -rtsopts'`

and run it by

`./banking`
