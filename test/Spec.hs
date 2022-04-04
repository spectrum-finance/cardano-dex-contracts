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
  [ checkSwap
  , checkSwapRedeemer
  , checkSwapIdentity
  , checkDeposit
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward
  , checkPoolDeposit
  , checkPoolDepositRedeemer
  ]