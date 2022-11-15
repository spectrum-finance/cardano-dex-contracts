{-# LANGUAGE OverloadedStrings #-}

module Tests.Vesting where

import qualified ErgoDex.PContracts.PVesting     as PVesting
import qualified ErgoDex.Contracts.Proxy.Vesting as PPVesting
import ErgoDex.PValidators

import Eval
import Gen.Utils
import Gen.VestingGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Time

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH
import Hedgehog.Range
import Hedgehog.Gen

import Gen.Models

checkVesting = testGroup "CheckVesting"
  [ HH.testProperty "correct_vesting" correctVesting
  , HH.testProperty "incorrect_signature_vesting" incorrectSignatureVesting
  , HH.testProperty "incorrect_deadline_vesting" incorrectDeadlineVesting
  , HH.testProperty "incorrect_vesting_input_idx" incorrectVestingInIdx
  , HH.testProperty "incorrect_vesting_reward_idx" incorrectVestingRewardIdx
  ]

correctVesting :: Property
correctVesting = property $ do
  let
    range = exponential 10 512
  pkh             <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  deadlineInt     <- forAll $ int range
  let
    deadline = toInteger deadlineInt

    vestingCfg     = genVestingConfig deadline pkh vestingAC
    vestingCfgData = toData vestingCfg
    vestingDatum   = OutputDatum $ mkDatum vestingCfg
    vestingTxIn    = genVestingTxIn vestingOutTxRef vestingDatum vestingAC 10
    userTxOut      = genUserTxOut vestingAC 10 pkh
  
    txInfo  = mkVestingTxInfo [vestingTxIn] [userTxOut] (deadline + 5) (deadline + 10) pkh
    purpose = mkPurpose vestingOutTxRef

    cxtData             = toData $ mkContext txInfo purpose
    vestingRedeemToData = toData $ PPVesting.VestingRedeemer 0 0

    result = eraseRight $ evalWithArgs (wrapValidator PVesting.vestingValidatorT) [vestingCfgData, vestingRedeemToData, cxtData]
  result === Right ()

incorrectSignatureVesting :: Property
incorrectSignatureVesting = property $ do
  let
    range = exponential 10 512
  pkh             <- forAll genPkh
  incorrectPkh    <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  deadlineInt     <- forAll $ int range
  let
    deadline = toInteger deadlineInt

    vestingCfg     = genVestingConfig deadline pkh vestingAC
    vestingCfgData = toData vestingCfg
    vestingDatum   = OutputDatum $ mkDatum vestingCfg
    vestingTxIn    = genVestingTxIn vestingOutTxRef vestingDatum vestingAC 10
    userTxOut      = genUserTxOut vestingAC 10 pkh
  
    txInfo  = mkVestingTxInfo [vestingTxIn] [userTxOut] (deadline + 5) (deadline + 10) incorrectPkh
    purpose = mkPurpose vestingOutTxRef

    cxtData             = toData $ mkContext txInfo purpose
    vestingRedeemToData = toData $ PPVesting.VestingRedeemer 0 0

    result = eraseLeft $ evalWithArgs (wrapValidator PVesting.vestingValidatorT) [vestingCfgData, vestingRedeemToData, cxtData]
  result === Left ()

incorrectDeadlineVesting :: Property
incorrectDeadlineVesting = property $ do
  let
    range = exponential 10 512
  pkh             <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  deadlineInt     <- forAll $ int range
  let
    deadline = toInteger deadlineInt

    vestingCfg     = genVestingConfig deadline pkh vestingAC
    vestingCfgData = toData vestingCfg
    vestingDatum   = OutputDatum $ mkDatum vestingCfg
    vestingTxIn    = genVestingTxIn vestingOutTxRef vestingDatum vestingAC 10
    userTxOut      = genUserTxOut vestingAC 10 pkh
  
    txInfo  = mkVestingTxInfo [vestingTxIn] [userTxOut] (deadline - 10) (deadline - 5) pkh
    purpose = mkPurpose vestingOutTxRef

    cxtData             = toData $ mkContext txInfo purpose
    vestingRedeemToData = toData $ PPVesting.VestingRedeemer 0 0

    result = eraseLeft $ evalWithArgs (wrapValidator PVesting.vestingValidatorT) [vestingCfgData, vestingRedeemToData, cxtData]
  result === Left ()

incorrectVestingInIdx :: Property
incorrectVestingInIdx = property $ do
  let
    range = exponential 10 512
  pkh             <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  userOutTxRef    <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  deadlineInt     <- forAll $ int range
  let
    deadline = toInteger deadlineInt

    vestingCfg     = genVestingConfig deadline pkh vestingAC
    vestingCfgData = toData vestingCfg
    vestingDatum   = OutputDatum $ mkDatum vestingCfg
    vestingTxIn    = genVestingTxIn vestingOutTxRef vestingDatum vestingAC 10
    userTxIn       = genTxInWithEmptyDatum userOutTxRef 100 pkh
    userTxOut      = genUserTxOut vestingAC 10 pkh
  
    txInfo  = mkVestingTxInfo [userTxIn, vestingTxIn] [userTxOut] (deadline + 5) (deadline + 10) pkh
    purpose = mkPurpose vestingOutTxRef

    cxtData             = toData $ mkContext txInfo purpose
    vestingRedeemToData = toData $ PPVesting.VestingRedeemer 0 0

    result = eraseLeft $ evalWithArgs (wrapValidator PVesting.vestingValidatorT) [vestingCfgData, vestingRedeemToData, cxtData]
  result === Left ()

incorrectVestingRewardIdx :: Property
incorrectVestingRewardIdx = property $ do
  let
    range = exponential 10 512
  pkh             <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  userOutTxRef    <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  deadlineInt     <- forAll $ int range
  let
    deadline = toInteger deadlineInt

    vestingCfg     = genVestingConfig deadline pkh vestingAC
    vestingCfgData = toData vestingCfg
    vestingDatum   = OutputDatum $ mkDatum vestingCfg
    vestingTxIn    = genVestingTxIn vestingOutTxRef vestingDatum vestingAC 10
    userTxOut      = genUserTxOut vestingAC 10 pkh

    userTxOutWithIncorrectValue = genUserTxOut vestingAC 50 pkh
  
    txInfo  = mkVestingTxInfo [vestingTxIn] [userTxOutWithIncorrectValue, userTxOut] (deadline + 5) (deadline + 10) pkh
    purpose = mkPurpose vestingOutTxRef

    cxtData             = toData $ mkContext txInfo purpose
    vestingRedeemToData = toData $ PPVesting.VestingRedeemer 0 0

    result = eraseLeft $ evalWithArgs (wrapValidator PVesting.vestingValidatorT) [vestingCfgData, vestingRedeemToData, cxtData]
  result === Left ()