module Main(main) where

import Tests.Deposit 
import Tests.Pool 
import Tests.Swap

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests = testGroup "Contracts"
  [ checkSwap
  --   checkDeposit
  -- , checkDepositRedeemer
  -- , checkDepositIdentity
  -- , checkDepositLq
  -- , checkDepositTokenReward
  -- , checkPoolDeposit
  -- , checkPoolDepositRedeemer
  ]