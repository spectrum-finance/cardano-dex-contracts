module Main(main) where

import Tests.Deposit 
import Tests.Pool 
import Tests.Swap
import Tests.Redeem

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ checkPool
  , checkPoolRedeemer
  , checkRedeem
  , checkRedeemIdentity
  , checkRedeemIsFair
  , checkRedeemRedeemer
  , checkDeposit 
  , checkDepositChange
  , checkDepositRedeemer
  , checkDepositIdentity
  , checkDepositLq
  , checkDepositTokenReward
  , checkSwap
  , checkSwapRedeemer
  , checkSwapIdentity
  ]