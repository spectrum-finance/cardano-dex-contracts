module Main(main) where

import Tests.Deposit 
import Tests.Pool 
import Tests.Swap

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ checkDeposit 
  , checkDepositChange
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward

  , checkPool
  , checkPoolRedeemer

  , checkSwap
  , checkSwapRedeemer
  , checkSwapIdentity
  ]