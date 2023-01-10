module Tests.LqMining.Simple.Deposit where

import ErgoDex.PValidators
import ErgoDex.PMintingValidators
import ErgoDex.Contracts.Proxy.LqMining.Simple.Deposit as D
import ErgoDex.PContracts.LqMining.Simple.PDeposit as PD

import Eval
import Gen.Utils
import Gen.Models (scriptCurrencySymbol)
import PlutusTx.Builtins.Internal
import qualified Data.ByteString as BS

import Hedgehog
import Hedgehog.Range
import Hedgehog.Gen

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.LqMining.Simple.DepositGen
import Gen.LqMining.Simple.StakingBundleGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V2.Contexts
import Plutarch.Lift
import PExtra.Ada
import qualified PExtra.API as API
import Debug.Trace

checkLMDeposit = testGroup "CheckLMDepositContract"
  [ HH.testProperty "deposit_is_correct" successDeposit 
  , HH.testProperty "incorrect_user_redeemer_value" incorrectRedeemerValue 
  , HH.testProperty "incorrect_user_redeemer_bundle_ac" incorrectRedeemerBundleAC 
  , HH.testProperty "incorrect_staking_bandle_datum_bundle_ac" incorrectStakingBundleDatumBundleAC 
  , HH.testProperty "incorrect_staking_bandle_datum_redeemer_pkh" incorrectStakingBundleDatumRedeemerPkh
  , HH.testProperty "incorrect_staking_bandle_invalid_tmp_qty" incorrectStakingBundleInvalidTmpQty
  , HH.testProperty "incorrect_staking_bandle_invalid_vlq_qty" incorrectStakingBundleInvalidVLQQty
  ]

successDeposit :: Property
successDeposit = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseRight $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Right ()

incorrectRedeemerValue :: Property
incorrectRedeemerValue = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 10 pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()

incorrectRedeemerBundleAC :: Property
incorrectRedeemerBundleAC = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  incorrectBundleCS <- forAll genCurrencySymbol

  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef incorrectBundleCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()

incorrectStakingBundleDatumBundleAC :: Property
incorrectStakingBundleDatumBundleAC = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  incorrectBundleAC <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft incorrectBundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()

incorrectStakingBundleDatumRedeemerPkh :: Property
incorrectStakingBundleDatumRedeemerPkh = property $ do
  pkh <- forAll genPkh
  invalidPkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC invalidPkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()

incorrectStakingBundleInvalidTmpQty :: Property
incorrectStakingBundleInvalidTmpQty = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC vlqQty tmpAC 1 stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()

incorrectStakingBundleInvalidVLQQty :: Property
incorrectStakingBundleInvalidVLQQty = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  vlqAC   <- forAll genAssetClass
  tmpAC   <- forAll genAssetClass
  poolNft <- forAll genAssetClass

  bundleIdAC <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  depositInTxRef <- forAll genTxOutRef
  
  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range
  let
    expectedNumEpochs = toInteger expectedNumEpochsInt

    bundleLqMP = lmPoolLqMintValidator poolNft 0x7fffffffffffffff
    bundleLQCS = scriptCurrencySymbol bundleLqMP

    vlqQty = toInteger vlqQtyInt
    tmpQty = vlqQty * expectedNumEpochs
  
    depositCfg     = genDepositConfig expectedNumEpochs bundleLQCS pkh vlqAC tmpAC
    depositCfgData = toData depositCfg
    depositDatum   = OutputDatum $ mkDatum depositCfg
    depositTxIn    = genDepositTxInInfo depositInTxRef vlqAC vlqQty tmpAC tmpQty depositDatum

    (redeemTxOut, bundleAC) = genRedeemerTxOut poolInTxRef bundleLQCS 0x7fffffffffffffff pkh

    fakePoolTxIn = genFakePoolTxInInfo poolInTxRef

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleAC vlqAC tmpAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg
    stakingBundleTxOut  = genStakingBundleTxOut vlqAC 1 tmpAC tmpQty stakingBundleDatum

    txInfo  = mkLMDepositTxInfo [fakePoolTxIn, depositTxIn] [redeemTxOut, stakingBundleTxOut]
    purpose = mkPurpose depositInTxRef
    cxtData = toData $ mkContext txInfo purpose

    depositRedeemConfigToData = toData $ D.DepositRedeemer 0 1 0 1
  
    result = eraseLeft $ evalWithArgs (wrapValidator PD.depositValidatorT) [depositCfgData, depositRedeemConfigToData, cxtData]
  result === Left ()