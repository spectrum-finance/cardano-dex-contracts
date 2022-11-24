module Tests.LqMining.Simple.Redeem where

import ErgoDex.PValidators
import ErgoDex.Contracts.Proxy.LqMining.Simple.Redeem as PR
import ErgoDex.PContracts.LqMining.Simple.PRedeem as R

import Eval
import Gen.Utils

import Hedgehog
import Hedgehog.Range
import Hedgehog.Gen

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.LqMining.Simple.RedeemGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V2.Contexts
import Plutarch.Lift
import PExtra.Ada
import qualified PExtra.API as API

checkLMRedeem = testGroup "CheckLMRedeemContract"
  [ HH.testProperty "redeem_is_correct"         successRedeem 
  , HH.testProperty "redeem_incorrect_lq_ac"    incorrectLqACRedeem
  , HH.testProperty "redeem_incorrect_lq_qty"   incorrectLqQtyRedeem
  , HH.testProperty "redeem_incorrect_redeemer" incorrectRedeemer
  , HH.testProperty "redeem_incorrect_pkh"      incorrectPkhRedeem
  ]

successRedeem :: Property
successRedeem = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  expectedLQAC    <- forAll genAssetClass
  poolNFTAC       <- forAll genAssetClass
  redeemOutTxRef  <- forAll genTxOutRef

  expectedLQAmountInt <- forAll $ int range
  let
    expectedLQAmount = toInteger expectedLQAmountInt
    
    redeemCfg     = genRedeemConfig expectedLQAC expectedLQAmount pkh
    redeemCfgData = toData redeemCfg
    redeemDatum   = OutputDatum $ mkDatum redeemCfg
    redeemTxIn    = genRedeemTxIn redeemOutTxRef redeemDatum
    userTxOut     = genUserTxOut expectedLQAC expectedLQAmount pkh

    txInfo  = mkLQRedeemTxInfo [redeemTxIn] [userTxOut]
    purpose = mkPurpose redeemOutTxRef

    cxtData            = toData $ mkContext txInfo purpose
    redeemConfigToData = toData $ PR.RedeemRedeemerConfig 0
  
    result = eraseRight $ evalWithArgs (wrapValidator R.redeemValidatorT) [redeemCfgData, redeemConfigToData, cxtData]
  result === Right ()

incorrectLqACRedeem :: Property
incorrectLqACRedeem = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  expectedLQAC    <- forAll genAssetClass
  incorrectLQAC   <- forAll genAssetClass
  redeemOutTxRef  <- forAll genTxOutRef

  expectedLQAmountInt <- forAll $ int range
  let
    expectedLQAmount = toInteger expectedLQAmountInt

    redeemCfg     = genRedeemConfig expectedLQAC expectedLQAmount pkh
    redeemCfgData = toData redeemCfg
    redeemDatum   = OutputDatum $ mkDatum redeemCfg
    redeemTxIn    = genRedeemTxIn redeemOutTxRef redeemDatum
    userTxOut     = genUserTxOut incorrectLQAC expectedLQAmount pkh

    txInfo  = mkLQRedeemTxInfo [redeemTxIn] [userTxOut]
    purpose = mkPurpose redeemOutTxRef

    cxtData            = toData $ mkContext txInfo purpose
    redeemConfigToData = toData $ PR.RedeemRedeemerConfig 0
  
    result = eraseLeft $ evalWithArgs (wrapValidator R.redeemValidatorT) [redeemCfgData, redeemConfigToData, cxtData]
  result === Left ()

incorrectPkhRedeem :: Property
incorrectPkhRedeem = property $ do
  pkh <- forAll genPkh

  incorrectPkh <- forAll genPkh
  let
    range = exponential 10 512

  expectedLQAC    <- forAll genAssetClass
  redeemOutTxRef  <- forAll genTxOutRef

  expectedLQAmountInt <- forAll $ int range
  let
    expectedLQAmount = toInteger expectedLQAmountInt

    redeemCfg     = genRedeemConfig expectedLQAC expectedLQAmount pkh
    redeemCfgData = toData redeemCfg
    redeemDatum   = OutputDatum $ mkDatum redeemCfg
    redeemTxIn    = genRedeemTxIn redeemOutTxRef redeemDatum
    userTxOut     = genUserTxOut expectedLQAC expectedLQAmount incorrectPkh

    txInfo  = mkLQRedeemTxInfo [redeemTxIn] [userTxOut]
    purpose = mkPurpose redeemOutTxRef

    cxtData            = toData $ mkContext txInfo purpose
    redeemConfigToData = toData $ PR.RedeemRedeemerConfig 0
  
    result = eraseLeft $ evalWithArgs (wrapValidator R.redeemValidatorT) [redeemCfgData, redeemConfigToData, cxtData]
  result === Left ()

incorrectLqQtyRedeem :: Property
incorrectLqQtyRedeem = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  expectedLQAC    <- forAll genAssetClass
  redeemOutTxRef  <- forAll genTxOutRef

  expectedLQAmountInt <- forAll $ int range
  let
    expectedLQAmount = toInteger expectedLQAmountInt

    redeemCfg     = genRedeemConfig expectedLQAC expectedLQAmount pkh
    redeemCfgData = toData redeemCfg
    redeemDatum   = OutputDatum $ mkDatum redeemCfg
    redeemTxIn    = genRedeemTxIn redeemOutTxRef redeemDatum
    userTxOut     = genUserTxOut expectedLQAC (expectedLQAmount - 1) pkh

    txInfo  = mkLQRedeemTxInfo [redeemTxIn] [userTxOut]
    purpose = mkPurpose redeemOutTxRef

    cxtData            = toData $ mkContext txInfo purpose
    redeemConfigToData = toData $ PR.RedeemRedeemerConfig 0
  
    result = eraseLeft $ evalWithArgs (wrapValidator R.redeemValidatorT) [redeemCfgData, redeemConfigToData, cxtData]
  result === Left ()

incorrectRedeemer :: Property
incorrectRedeemer = property $ do
  pkh        <- forAll genPkh
  anotherPkh <- forAll genPkh
  let
    range = exponential 10 512

  expectedLQAC    <- forAll genAssetClass
  nonLQAC         <- forAll genAssetClass
  redeemOutTxRef  <- forAll genTxOutRef

  expectedLQAmountInt <- forAll $ int range
  let
    expectedLQAmount = toInteger expectedLQAmountInt

    redeemCfg     = genRedeemConfig expectedLQAC expectedLQAmount pkh
    redeemCfgData = toData redeemCfg
    redeemDatum   = OutputDatum $ mkDatum redeemCfg
    redeemTxIn    = genRedeemTxIn redeemOutTxRef redeemDatum
    userTxOut     = genUserTxOut expectedLQAC expectedLQAmount pkh
    anotherTxOut  = genUserTxOut nonLQAC 100 anotherPkh

    txInfo  = mkLQRedeemTxInfo [redeemTxIn] [userTxOut, anotherTxOut]
    purpose = mkPurpose redeemOutTxRef

    cxtData            = toData $ mkContext txInfo purpose
    redeemConfigToData = toData $ PR.RedeemRedeemerConfig 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator R.redeemValidatorT) [redeemCfgData, redeemConfigToData, cxtData]
  result === Left ()