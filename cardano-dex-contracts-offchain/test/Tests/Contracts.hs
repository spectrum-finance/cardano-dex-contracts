module Tests.Contracts where

import Control.Monad

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import ErgoDex.PValidators

checkContractsRecovering = testGroup "ContractsRecovering"
  [ HH.testProperty "swap_contract_recovering" swapRecovering
  , HH.testProperty "pool_contract_recovering" poolRecovering
  , HH.testProperty "deposit_contract_recovering" depositRecovering
  , HH.testProperty "redeem_contract_recovering" redeemRecovering
  , HH.testProperty "vesting_contract_recovering" redeemRecovering
  , HH.testProperty "vesting_with_period_contract_recovering" redeemRecovering
  ]

swapRecovering :: Property
swapRecovering = withTests 1 . property $ evalIO (void (swapValidator))

depositRecovering :: Property
depositRecovering = withTests 1 . property $ evalIO (void depositValidator)

redeemRecovering :: Property
redeemRecovering = withTests 1 . property $ evalIO (void redeemValidator)

poolRecovering :: Property
poolRecovering = withTests 1 . property $ evalIO (void poolValidator)

vestingRecovering :: Property
vestingRecovering = withTests 1 . property $ evalIO (void vestingValidator)

vestingWithPeriodRecovering :: Property
vestingWithPeriodRecovering = withTests 1 . property $ evalIO (void vestingWithPeriodValidator)

