module Main(main) where

import ErgoDex.PMintingValidators

import Tests.Deposit
import Tests.Pool 
import Tests.Swap
import Tests.Redeem
import Tests.Staking
import Tests.StakeMinting
import Tests.Api

import Test.Tasty
import Test.Tasty.HUnit

import ErgoDex.PValidators
import PlutusLedgerApi.V2 as PV2
import Plutarch.Api.V2

main :: IO ()
main = do
  defaultMain tests

tests = testGroup "Contracts"
  [ checkPValueLength
  , checkStakeChangeMintingPolicy
  , checkPool
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
  , checkPkhLockStaking
  ]