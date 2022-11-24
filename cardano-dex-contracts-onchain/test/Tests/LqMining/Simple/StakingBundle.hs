module Tests.LqMining.Simple.StakingBundle where

import ErgoDex.PValidators
import ErgoDex.PContracts.LqMining.Simple.PStakingBundle as SB
import ErgoDex.Contracts.Proxy.LqMining.Simple.StakingBundle as PSB

import Eval
import Gen.Utils

import Hedgehog
import Hedgehog.Range
import Hedgehog.Gen
import Hedgehog.Internal.Property (TestLimit(..))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.LqMining.Simple.LMPoolGen
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

checkLMStakingBundle = testGroup "CheckLMStakingBundleContract"
  [ HH.testProperty "success_compound" successCompound
  , HH.testProperty "incorrect_tmp_qty_in_successor" incorrectTmpQtyInSuccessor
  , HH.testProperty "incorrect_vlq_qty_in_successor" incorrectVLQQtyInSuccessor
  , HH.testProperty "success_redeem" successRedeem
  , HH.testProperty "incorrect_redeem" incorrectRedeem
  ]

successCompound :: Property
successCompound = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  -- common acs
  bundleIdAC  <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  -- poolConfig
  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range

  vlqQtyInt <- forAll $ int range

  poolX <- forAll genAssetClass
  
  poolInTxRef <- forAll genTxOutRef

  stakingBundleTxRef <- forAll genTxOutRef
  let
    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = (toInteger programBudgetInt) + epochNum
    execBudget    = toInteger execBudgetInt

    vlqQty = toInteger vlqQtyInt

    expectedNumEpochs = epochNum - 1

    tmpQty = vlqQty * expectedNumEpochs

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleLQAC bundleVLQAC bundleTMPAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg

    stakingBundleDatumToData = toData stakingBundleCfg

    stakingBundleTxOutForTxIn  = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC tmpQty stakingBundleDatum
    stakingBundleTxIn   = mkTxIn stakingBundleTxRef stakingBundleTxOutForTxIn

    compoundEpoch = 2

    releasedTMP = tmpQty - ((epochNum - compoundEpoch) * vlqQty)

    stakingBundleTxOut = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC (tmpQty - releasedTMP) stakingBundleDatum
  
    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 900000000000 10 10 0 poolInCfg

    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 800000000000 10 10 0 poolOutCfg

    epochRewardTotal = programBudget `div` epochNum

    epochsToCompound = epochNum - compoundEpoch

    epochsBurned = (tmpQty `div` vlqQty) - epochsToCompound

    reward = (epochRewardTotal * vlqQty * epochsBurned) `div` 10

    userRewardOut = genUserTxOut poolX reward pkh

    txInfo  = mkLMStakingBundleTxInfo [stakingBundleTxIn, poolTxIn] [poolTxOut, stakingBundleTxOut, userRewardOut]
    purpose = mkPurpose stakingBundleTxRef
    cxtData = toData $ mkContext txInfo purpose

    stakingBundleConfigToData = toData $ PSB.StakingBundleRedeemer 1 0 0 2 1

    result = eraseRight $ evalWithArgs (wrapValidator SB.stakingBundleValidatorT) [stakingBundleDatumToData, stakingBundleConfigToData, cxtData]
    
  result === Right ()

incorrectTmpQtyInSuccessor :: Property
incorrectTmpQtyInSuccessor = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  -- common acs
  bundleIdAC  <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  -- poolConfig
  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range

  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range

  poolX <- forAll genAssetClass
  
  poolInTxRef <- forAll genTxOutRef

  stakingBundleTxRef <- forAll genTxOutRef
  let
    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = toInteger programBudgetInt
    execBudget    = toInteger execBudgetInt

    vlqQty = toInteger vlqQtyInt

    expectedNumEpochs = epochNum - 1

    tmpQty = vlqQty * expectedNumEpochs

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleLQAC bundleVLQAC bundleTMPAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg

    stakingBundleDatumToData = toData stakingBundleCfg

    stakingBundleTxOutForTxIn  = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC tmpQty stakingBundleDatum
    stakingBundleTxIn   = mkTxIn stakingBundleTxRef stakingBundleTxOutForTxIn

    compoundEpoch = 2

    releasedTMP = tmpQty - ((epochNum - compoundEpoch) * vlqQty)

    stakingBundleTxOut = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC (tmpQty - releasedTMP - 2) stakingBundleDatum
  
    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 900000000000 100 0x7fffffffffffffff 0 poolInCfg

    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 800000000000 100 0x7fffffffffffffff 0 poolOutCfg

    epochRewardTotal = programBudget `div` epochNum

    epochsToCompound = epochNum - compoundEpoch

    epochsBurned = (tmpQty `div` vlqQty) - epochsToCompound

    reward = (epochRewardTotal * vlqQty * epochsBurned) `div` 0x7fffffffffffffff

    userRewardOut = genUserTxOut poolX reward pkh

    txInfo  = mkLMStakingBundleTxInfo [stakingBundleTxIn, poolTxIn] [poolTxOut, stakingBundleTxOut, userRewardOut]
    purpose = mkPurpose stakingBundleTxRef
    cxtData = toData $ mkContext txInfo purpose

    stakingBundleConfigToData = toData $ PSB.StakingBundleRedeemer 1 0 0 2 1

    result = eraseLeft $ evalWithArgs (wrapValidator SB.stakingBundleValidatorT) [stakingBundleDatumToData, stakingBundleConfigToData, cxtData]
    
  result === Left ()

incorrectVLQQtyInSuccessor :: Property
incorrectVLQQtyInSuccessor = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  -- common acs
  bundleIdAC  <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  -- poolConfig
  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range

  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range

  poolX <- forAll genAssetClass
  
  poolInTxRef <- forAll genTxOutRef

  stakingBundleTxRef <- forAll genTxOutRef
  let
    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = toInteger programBudgetInt
    execBudget    = toInteger execBudgetInt

    vlqQty = toInteger vlqQtyInt

    expectedNumEpochs = epochNum - 1

    tmpQty = vlqQty * expectedNumEpochs

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleLQAC bundleVLQAC bundleTMPAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg

    stakingBundleDatumToData = toData stakingBundleCfg

    stakingBundleTxOutForTxIn  = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC tmpQty stakingBundleDatum
    stakingBundleTxIn   = mkTxIn stakingBundleTxRef stakingBundleTxOutForTxIn

    compoundEpoch = 2

    releasedTMP = tmpQty - ((epochNum - compoundEpoch) * vlqQty)

    stakingBundleTxOut = genStakingBundleTxOut bundleVLQAC (vlqQty - 1) bundleTMPAC (tmpQty - releasedTMP) stakingBundleDatum
  
    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 900000000000 100 0x7fffffffffffffff 0 poolInCfg

    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 800000000000 100 0x7fffffffffffffff 0 poolOutCfg

    epochRewardTotal = programBudget `div` epochNum
    epochsToCompound = epochNum - compoundEpoch

    epochsBurned = (tmpQty `div` vlqQty) - epochsToCompound

    reward = (epochRewardTotal * vlqQty * epochsBurned) `div` 0x7fffffffffffffff

    userRewardOut = genUserTxOut poolX reward pkh

    txInfo  = mkLMStakingBundleTxInfo [stakingBundleTxIn, poolTxIn] [poolTxOut, stakingBundleTxOut, userRewardOut]
    purpose = mkPurpose stakingBundleTxRef
    cxtData = toData $ mkContext txInfo purpose

    stakingBundleConfigToData = toData $ PSB.StakingBundleRedeemer 1 0 0 2 1

    result = eraseLeft $ evalWithArgs (wrapValidator SB.stakingBundleValidatorT) [stakingBundleDatumToData, stakingBundleConfigToData, cxtData]
    
  result === Left ()

successRedeem :: Property
successRedeem = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  -- common acs
  bundleIdAC  <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  -- poolConfig
  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range

  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range

  poolX <- forAll genAssetClass
  
  poolInTxRef  <- forAll genTxOutRef
  userInTxRef  <- forAll genTxOutRef

  stakingBundleTxRef <- forAll genTxOutRef
  let
    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = toInteger programBudgetInt
    execBudget    = toInteger execBudgetInt

    vlqQty = toInteger vlqQtyInt

    expectedNumEpochs = epochNum - 1

    tmpQty = vlqQty * expectedNumEpochs

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleLQAC bundleVLQAC bundleTMPAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg

    stakingBundleDatumToData = toData stakingBundleCfg

    stakingBundleTxOutForTxIn  = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC tmpQty stakingBundleDatum
    stakingBundleTxIn   = mkTxIn stakingBundleTxRef stakingBundleTxOutForTxIn

    compoundEpoch = 2

    releasedTMP = tmpQty - ((epochNum - compoundEpoch) * vlqQty)

    stakingBundleTxOut = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC (tmpQty - releasedTMP) stakingBundleDatum
  
    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 900000000000 100 0x7fffffffffffffff 0 poolInCfg

    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 900000000000 90 0x7fffffffffffffff 0 poolOutCfg

    epochRewardTotal = programBudget `div` epochNum

    epochsToCompound = epochNum - compoundEpoch

    epochsBurned = (tmpQty `div` vlqQty) - epochsToCompound

    reward = (epochRewardTotal * vlqQty * epochsBurned) `div` 0x7fffffffffffffff

    userRewardOut = genUserTxOut poolX reward pkh

    userTxOutForTxIn = genUserTxOut bundleLQAC 0x7fffffffffffffff pkh
    userUserTxIn = mkTxIn userInTxRef userTxOutForTxIn

    txInfo  = mkLMStakingBundleTxInfo [stakingBundleTxIn, poolTxIn, userUserTxIn] [poolTxOut, stakingBundleTxOut, userRewardOut]
    purpose = mkPurpose stakingBundleTxRef
    cxtData = toData $ mkContext txInfo purpose

    stakingBundleConfigToData = toData $ PSB.StakingBundleRedeemer 1 2 0 2 1

    result = eraseRight $ evalWithArgs (wrapValidator SB.stakingBundleValidatorT) [stakingBundleDatumToData, stakingBundleConfigToData, cxtData]
    
  result === Right ()

incorrectRedeem :: Property
incorrectRedeem = property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  -- common acs
  bundleIdAC  <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  -- poolConfig
  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range

  vlqQtyInt <- forAll $ int range

  expectedNumEpochsInt <- forAll $ int range

  poolX <- forAll genAssetClass
  
  poolInTxRef    <- forAll genTxOutRef
  userInTxRef    <- forAll genTxOutRef

  stakingBundleTxRef <- forAll genTxOutRef
  let
    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = toInteger programBudgetInt
    execBudget    = toInteger execBudgetInt

    vlqQty = toInteger vlqQtyInt

    expectedNumEpochs = epochNum - 1

    tmpQty = vlqQty * expectedNumEpochs

    stakingBundleCfg    = genStakingBundleConfig bundleIdAC poolNft bundleLQAC bundleVLQAC bundleTMPAC pkh
    stakingBundleDatum  = OutputDatum $ mkDatum stakingBundleCfg

    stakingBundleDatumToData = toData stakingBundleCfg

    stakingBundleTxOutForTxIn  = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC tmpQty stakingBundleDatum
    stakingBundleTxIn   = mkTxIn stakingBundleTxRef stakingBundleTxOutForTxIn

    compoundEpoch = 2

    releasedTMP = tmpQty - ((epochNum - compoundEpoch) * vlqQty)

    stakingBundleTxOut = genStakingBundleTxOut bundleVLQAC vlqQty bundleTMPAC (tmpQty - releasedTMP) stakingBundleDatum
  
    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 900000000000 100 0x7fffffffffffffff 0 poolInCfg

    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 900000000000 500 0x7fffffffffffffff 0 poolOutCfg

    epochRewardTotal = programBudget `div` epochNum

    epochsToCompound = epochNum - compoundEpoch

    epochsBurned = (tmpQty `div` vlqQty) - epochsToCompound

    reward = (epochRewardTotal * vlqQty * epochsBurned) `div` 0x7fffffffffffffff

    userRewardOut = genUserTxOut poolX reward pkh

    userTxOutForTxIn = genUserTxOut bundleLQAC 1 pkh
    userUserTxIn = mkTxIn userInTxRef userTxOutForTxIn

    txInfo  = mkLMStakingBundleTxInfo [stakingBundleTxIn, poolTxIn, userUserTxIn] [poolTxOut, stakingBundleTxOut, userRewardOut]
    purpose = mkPurpose stakingBundleTxRef
    cxtData = toData $ mkContext txInfo purpose

    stakingBundleConfigToData = toData $ PSB.StakingBundleRedeemer 1 2 0 2 1

    result = eraseLeft $ evalWithArgs (wrapValidator SB.stakingBundleValidatorT) [stakingBundleDatumToData, stakingBundleConfigToData, cxtData]
    
  result === Left ()