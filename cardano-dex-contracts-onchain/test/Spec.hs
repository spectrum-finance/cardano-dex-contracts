module Main(main) where

import Tests.Deposit 
import Tests.Pool 
import Tests.Swap
import Tests.Redeem
import Tests.LqMining.Simple.StakingBundle
import Tests.LqMining.Simple.Redeem
import Tests.LqMining.Simple.Deposit
import Tests.LqMining.Simple.LMPool

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ checkLMPool
    -- checkLMStakingBundle
    -- checkLMDeposit
    --checkLMRedeem
    --checkPool
  -- , checkPoolRedeemer
  -- , checkRedeem
  -- , checkRedeemIdentity
  -- , checkRedeemIsFair
  -- , checkRedeemRedeemer
  -- , checkDeposit 
  -- , checkDepositChange
  -- , checkDepositRedeemer
  -- , checkDepositIdentity
  -- , checkDepositLq
  -- , checkDepositTokenReward
  -- , checkSwap
  -- , checkSwapRedeemer
  -- , checkSwapIdentity
  ]