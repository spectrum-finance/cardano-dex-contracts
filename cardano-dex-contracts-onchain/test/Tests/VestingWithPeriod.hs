{-# LANGUAGE OverloadedStrings #-}

module Tests.VestingWithPeriod where

import qualified ErgoDex.PContracts.PVestingWithPeriod      as PVestingWP
import qualified ErgoDex.Contracts.Proxy.VestingWithPeriod  as PPVestingWP
import ErgoDex.PValidators

import Eval
import Gen.Utils
import Gen.VestingWithPeriodGen

import Plutarch.Prelude
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Time

import Hedgehog

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH
import Hedgehog.Range
import Hedgehog.Gen

import Prelude
import Data.Either

import Gen.Models
import Debug.Trace

checkVestingWithPeriod = testGroup "checkVestingWithPeriod"
  [ HH.testProperty "correct_vesting" correctVesting
  , HH.testProperty "incorrect_signature_qty_vesting" incorrectSignatureQtyVesting
  , HH.testProperty "incorrect_time_vesting" incorrectTimeVesting
  , HH.testProperty "incorrect_value_vesting" incorrectValueVesting
  , HH.testProperty "incorrect_datum_in_output" incorrectDatumVesting
  ]

correctVesting :: Property
correctVesting = property $ do
  let
    range = linear 24 4096
  firstPkh        <- forAll genPkh
  secondPkh       <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  vestingStartInt <- (forAll $ int range)

  vestingPeriodDurationInt <- forAll $ int range

  totalVestedInt  <- forAll $ int range
  periodVetedInt  <- forAll $ int range
  let
    vestingStart = toInteger vestingStartInt

    vestingPeriodDuration = toInteger vestingPeriodDurationInt
    
    totalVested  = toInteger totalVestedInt
    periodVested = toInteger periodVetedInt

    maxPeriodId = totalVested `div` periodVested

    vestingWPCfg          = genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested [firstPkh, secondPkh] vestingAC
    vestingWPCfgData      = toData vestingWPCfg
    vestingWPDatum        = OutputDatum $ mkDatum vestingWPCfg
    initialVestingWPTxIn  = genVestingWPTxIn vestingOutTxRef vestingWPDatum vestingAC totalVested 2
    periodsList           = [1..maxPeriodId]
  
    (_, result) = 
      foldl (\(vestingBox, prevResult) periodId -> do
          let
            deadline      = vestingStart + vestingPeriodDuration * periodId

            vestingRedeemToData = toData $ PPVestingWP.VestingWithPeriodRedeemer 0 periodId

            newVestingBox = genVestingWPTxOut vestingWPDatum vestingAC (totalVested - periodId * periodVested) 2
            
            newVestingBoxInput = mkTxIn vestingOutTxRef newVestingBox

            toVest = if (periodId == maxPeriodId) then (totalVested - periodId * periodVested) else periodVested

            userTxOut     = genUserTxOut vestingAC toVest firstPkh

            txInfo        =
              if (periodId == maxPeriodId) 
              then mkVestingTxInfo [vestingBox] [userTxOut] (deadline + 5) (deadline + 10) [firstPkh, secondPkh]
              else mkVestingTxInfo [vestingBox] [newVestingBox, userTxOut] (deadline + 5) (deadline + 10) [firstPkh, secondPkh]

            purpose      = mkPurpose vestingOutTxRef

            cxtData      = toData $ mkContext txInfo purpose

            resultEither = eraseRight $ evalWithArgs (wrapValidator (PVestingWP.vestingWithPeriodValidatorT (pconstant 2))) [vestingWPCfgData, vestingRedeemToData, cxtData]
            
           in (newVestingBoxInput, prevResult && isRight resultEither)
      ) (initialVestingWPTxIn, True) periodsList

  result === True

incorrectSignatureQtyVesting :: Property
incorrectSignatureQtyVesting = property $ do
  let
    range = linear 24 4096
  firstPkh        <- forAll genPkh
  secondPkh       <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  vestingStartInt <- (forAll $ int range)

  vestingPeriodDurationInt <- forAll $ int range

  totalVestedInt  <- forAll $ int range
  let
    vestedRange = linear 24 totalVestedInt

  maxPeriodIdInt <- forAll $ int vestedRange
  let

    vestingStart = toInteger vestingStartInt

    vestingPeriodDuration = toInteger vestingPeriodDurationInt
    
    totalVested = toInteger totalVestedInt
    maxPeriodId = toInteger maxPeriodIdInt

    periodVested = totalVested `div` maxPeriodId

    vestingWPCfg          = genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested [firstPkh, secondPkh] vestingAC
    vestingWPCfgData      = toData vestingWPCfg
    vestingWPDatum        = OutputDatum $ mkDatum vestingWPCfg
    initialVestingWPTxIn  = genVestingWPTxIn vestingOutTxRef vestingWPDatum vestingAC totalVested 2
    periodsList           = [1..maxPeriodId]
  
  let
    (_, result) = 
      foldl (\(vestingBox, prevResult) periodId ->
          let
            deadline      = vestingStart + vestingPeriodDuration * periodId

            vestingRedeemToData = toData $ PPVestingWP.VestingWithPeriodRedeemer 0 periodId

            newVestingBox = genVestingWPTxOut vestingWPDatum vestingAC (totalVested - periodId * periodVested) 2
            
            newVestingBoxInput = mkTxIn vestingOutTxRef newVestingBox

            userTxOut     = genUserTxOut vestingAC periodVested firstPkh

            txInfo        = 
              if (periodId == maxPeriodId) 
              then mkVestingTxInfo [vestingBox] [userTxOut] (deadline + 5) (deadline + 10) [secondPkh]
              else mkVestingTxInfo [vestingBox] [newVestingBox, userTxOut] (deadline + 5) (deadline + 10) [secondPkh]

            purpose      = mkPurpose vestingOutTxRef

            cxtData      = toData $ mkContext txInfo purpose

            resultEither = eraseRight $ evalWithArgs (wrapValidator (PVestingWP.vestingWithPeriodValidatorT (pconstant 2))) [vestingWPCfgData, vestingRedeemToData, cxtData]
            
          in (newVestingBoxInput, prevResult && isRight resultEither)
      ) (initialVestingWPTxIn, True) periodsList

  result === False

incorrectTimeVesting :: Property
incorrectTimeVesting = property $ do
  let
    range = linear 24 4096
  firstPkh        <- forAll genPkh
  secondPkh       <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  vestingStartInt <- (forAll $ int range)

  vestingPeriodDurationInt <- forAll $ int range

  totalVestedInt  <- forAll $ int range
  let
    vestedRange = linear 24 totalVestedInt

  maxPeriodIdInt <- forAll $ int vestedRange
  let

    vestingStart = toInteger vestingStartInt

    vestingPeriodDuration = toInteger vestingPeriodDurationInt
    
    totalVested = toInteger totalVestedInt
    maxPeriodId = toInteger maxPeriodIdInt

    periodVested = totalVested `div` maxPeriodId

    vestingWPCfg          = genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested [firstPkh, secondPkh] vestingAC
    vestingWPCfgData      = toData vestingWPCfg
    vestingWPDatum        = OutputDatum $ mkDatum vestingWPCfg
    initialVestingWPTxIn  = genVestingWPTxIn vestingOutTxRef vestingWPDatum vestingAC totalVested 1
    periodsList           = [1..maxPeriodId]
  let
    (_, result) = 
      foldl (\(vestingBox, prevResult) periodId ->
          let

            deadline      = vestingStart + vestingPeriodDuration * periodId

            vestingRedeemToData = toData $ PPVestingWP.VestingWithPeriodRedeemer 0 periodId

            newVestingBox = genVestingWPTxOut vestingWPDatum vestingAC (totalVested - periodId * periodVested) 1
            
            newVestingBoxInput = mkTxIn vestingOutTxRef newVestingBox

            userTxOut     = genUserTxOut vestingAC (periodVested) firstPkh

            txInfo        = 
              if (periodId == maxPeriodId) 
              then mkVestingTxInfo [vestingBox] [userTxOut] (deadline - 5) (deadline + 5) [firstPkh, secondPkh]
              else mkVestingTxInfo [vestingBox] [newVestingBox, userTxOut] (deadline - 5) (deadline + 5) [secondPkh]

            purpose      = mkPurpose vestingOutTxRef

            cxtData      = toData $ mkContext txInfo purpose

            resultEither = eraseRight $ evalWithArgs (wrapValidator (PVestingWP.vestingWithPeriodValidatorT (pconstant 1))) [vestingWPCfgData, vestingRedeemToData, cxtData]
            
          in (newVestingBoxInput, prevResult && isRight resultEither)
      ) (initialVestingWPTxIn, True) periodsList

  result === False

incorrectValueVesting :: Property
incorrectValueVesting = property $ do
  let
    range = linear 24 4096
  firstPkh        <- forAll genPkh
  secondPkh       <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  vestingStartInt <- (forAll $ int range)

  vestingPeriodDurationInt <- forAll $ int range

  totalVestedInt  <- forAll $ int range
  let
    vestedRange = linear 24 totalVestedInt

  maxPeriodIdInt <- forAll $ int vestedRange
  let

    vestingStart = toInteger vestingStartInt

    vestingPeriodDuration = toInteger vestingPeriodDurationInt

    totalVested = toInteger totalVestedInt
    maxPeriodId = toInteger maxPeriodIdInt

    periodVested = totalVested `div` maxPeriodId

    vestingWPCfg          = genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested [firstPkh, secondPkh] vestingAC
    vestingWPCfgData      = toData vestingWPCfg
    vestingWPDatum        = OutputDatum $ mkDatum vestingWPCfg
    initialVestingWPTxIn  = genVestingWPTxIn vestingOutTxRef vestingWPDatum vestingAC totalVested 1
    periodsList           = [1..maxPeriodId]
  let
    (_, result) = 
      foldl (\(vestingBox, prevResult) periodId ->
          let
            deadline      = vestingStart + vestingPeriodDuration * periodId

            vestingRedeemToData = toData $ PPVestingWP.VestingWithPeriodRedeemer 0 periodId

            newVestingBox = 
              if (totalVested - periodId * periodVested >= 10) 
              then genVestingWPTxOut vestingWPDatum vestingAC (totalVested - periodId * periodVested - 10) 1
              else genVestingWPTxOut vestingWPDatum vestingAC (totalVested - periodId * periodVested) 1
            
            newVestingBoxInput = mkTxIn vestingOutTxRef newVestingBox

            userTxOut     = genUserTxOut vestingAC (periodVested + 10) firstPkh

            txInfo        = 
              if (periodId == maxPeriodId) 
              then mkVestingTxInfo [vestingBox] [userTxOut] (deadline + 5) (deadline + 10) [firstPkh, secondPkh]
              else mkVestingTxInfo [vestingBox] [newVestingBox, userTxOut] (deadline + 5) (deadline + 10) [secondPkh]

            purpose      = mkPurpose vestingOutTxRef

            cxtData      = toData $ mkContext txInfo purpose

            resultEither = eraseRight $ evalWithArgs (wrapValidator (PVestingWP.vestingWithPeriodValidatorT (pconstant 1))) [vestingWPCfgData, vestingRedeemToData, cxtData]
            
          in (newVestingBoxInput, prevResult && isRight resultEither)
      ) (initialVestingWPTxIn, True) periodsList

  result === False

incorrectDatumVesting :: Property
incorrectDatumVesting = property $ do
  let
    range = linear 24 4096
  firstPkh        <- forAll genPkh
  secondPkh       <- forAll genPkh
  vestingOutTxRef <- forAll genTxOutRef
  vestingAC       <- forAll genAssetClass
  vestingStartInt <- (forAll $ int range)

  vestingPeriodDurationInt <- forAll $ int range

  totalVestedInt  <- forAll $ int range
  let
    vestedRange = linear 24 totalVestedInt

  maxPeriodIdInt <- forAll $ int vestedRange
  let

    vestingStart = toInteger vestingStartInt

    vestingPeriodDuration = toInteger vestingPeriodDurationInt

    totalVested = toInteger totalVestedInt
    maxPeriodId = toInteger maxPeriodIdInt

    periodVested = totalVested `div` maxPeriodId
    periodId = 1

    vestingWPCfg          = genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested [firstPkh, secondPkh] vestingAC
    vestingWPCfgData      = toData vestingWPCfg
    vestingWPDatum        = OutputDatum $ mkDatum vestingWPCfg
    initialVestingWPTxIn  = genVestingWPTxIn vestingOutTxRef vestingWPDatum vestingAC totalVested 1
    periodsList           = [1..maxPeriodId]

    deadline      = vestingStart + vestingPeriodDuration * periodId

    vestingRedeemToData = toData $ PPVestingWP.VestingWithPeriodRedeemer 0 periodId

    newVestingBox = genVestingWPTxOut NoOutputDatum vestingAC (totalVested - periodId * periodVested) 1
    
    userTxOut    = genUserTxOut vestingAC periodVested firstPkh
    txInfo       = mkVestingTxInfo [initialVestingWPTxIn] [newVestingBox, userTxOut] (deadline + 5) (deadline + 10) [secondPkh]
    purpose      = mkPurpose vestingOutTxRef
    cxtData      = toData $ mkContext txInfo purpose
    resultEither = eraseLeft $ evalWithArgs (wrapValidator (PVestingWP.vestingWithPeriodValidatorT (pconstant 1))) [vestingWPCfgData, vestingRedeemToData, cxtData]
    
  resultEither === Left ()