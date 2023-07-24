module Tests.Staking where

import Plutarch.Prelude

import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PStakingValidators as PStakingValidators
import qualified ErgoDex.PContracts.PStakingWithPkhLock as PkhStaking
import ErgoDex.PValidators

import Eval
import Gen.Utils

import PlutusLedgerApi.V2

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.RedeemGen

checkPkhLockStaking = testGroup "CheckStakingWithPkhLock"
  [ HH.testProperty "correct_delegator_change_is_success" correctDelegatorChange
  , HH.testProperty "correct_delegator_change_is_success (Correct Threshold)" correctThreshold
  , HH.testProperty "incorrect_delecator_change_is_failed" incorrectDelegatorChange
  , HH.testProperty "incorrect_delecator_change_is_failed (Incorrect Threshold)" incorrectThreshold
  ]

correctDelegatorChange :: Property
correctDelegatorChange = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    sc = StakingHash (PubKeyCredential pkh)
    stakingContract = PkhStaking.pkhLockStakingValidatorT (pconstant [pkh]) (pconstant 1)
  
    txInfo  = mkTxInfoOnlyWithSignatures [pkh]
    purpose = mkDelegatingPurpose sc pkh

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (PStakingValidators.wrapStakingValidator stakingContract) [orderRedeemToData, cxtToData]

  result === Right ()

correctThreshold :: Property
correctThreshold = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh1             <- forAll genPkh
  pkh2             <- forAll genPkh
  pkh3             <- forAll genPkh
  stakingPkh       <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    sc = StakingHash (PubKeyCredential stakingPkh)
    stakingContract = PkhStaking.pkhLockStakingValidatorT (pconstant [pkh1, pkh2, pkh3]) (pconstant 2)
  
    txInfo  = mkTxInfoOnlyWithSignatures [pkh1, pkh2]
    purpose = mkDelegatingPurpose sc pkh1

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseRight $ evalWithArgs (PStakingValidators.wrapStakingValidator stakingContract) [orderRedeemToData, cxtToData]

  result === Right ()

incorrectDelegatorChange :: Property
incorrectDelegatorChange = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh             <- forAll genPkh
  incorrectPkh    <- forAll genPkh
  stakingPkh      <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    sc = StakingHash (PubKeyCredential stakingPkh)
    stakingContract = PkhStaking.pkhLockStakingValidatorT (pconstant [pkh]) (pconstant 1)
  
    txInfo  = mkTxInfoOnlyWithSignatures [incorrectPkh]
    purpose = mkDelegatingPurpose sc pkh

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseLeft $ evalWithArgs (PStakingValidators.wrapStakingValidator stakingContract) [orderRedeemToData, cxtToData]

  result === Left ()

incorrectThreshold :: Property
incorrectThreshold = property $ do
  let (x, y, nft, lq) = genAssetClasses
  pkh1             <- forAll genPkh
  pkh2             <- forAll genPkh
  pkh3             <- forAll genPkh
  stakingPkh       <- forAll genPkh
  orderTxRef      <- forAll genTxOutRef
  let
    sc = StakingHash (PubKeyCredential stakingPkh)
    stakingContract = PkhStaking.pkhLockStakingValidatorT (pconstant [pkh1, pkh2, pkh3]) (pconstant 2)
  
    txInfo  = mkTxInfoOnlyWithSignatures [pkh2]
    purpose = mkDelegatingPurpose sc pkh2

    cxtToData          = toData $ mkContext txInfo purpose
    orderRedeemToData  = toData $ mkOrderRedeemer 0 1 1

    result = eraseLeft $ evalWithArgs (PStakingValidators.wrapStakingValidator stakingContract) [orderRedeemToData, cxtToData]

  result === Left ()