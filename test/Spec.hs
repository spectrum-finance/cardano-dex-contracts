module Main(main) where

import Tests.Deposit 
import Tests.Pool 

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests = testGroup "Contracts"
  [ checkDeposit
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward
  , checkPoolDeposit
  , checkPoolDepositRedeemer
  ]