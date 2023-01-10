module Tests.LqMining.Simple.LMPool where

import ErgoDex.PValidators
import ErgoDex.PMintingValidators
import ErgoDex.Contracts.Proxy.LqMining.Simple.LMPool as PLM
import ErgoDex.PContracts.LqMining.Simple.PLMPool as LM

import Eval
import Gen.Utils
import Gen.Models (scriptCurrencySymbol)
import PlutusTx.Builtins.Internal
import qualified Data.ByteString as BS

import Hedgehog
import Hedgehog.Range
import Hedgehog.Gen
import Hedgehog.Internal.Property

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog as HH

import Gen.Models
import Gen.DepositGen
import Gen.PoolGen
import Gen.LqMining.Simple.DepositGen
import Gen.LqMining.Simple.StakingBundleGen
import Gen.LqMining.Simple.LMPoolGen

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

checkLMPool = testGroup "CheckLMPoolContract"
  [ HH.testProperty "deposit_is_correct" successDeposit
  , HH.testProperty "redeem_is_correct" successRedeem
  ]

successDeposit :: Property
successDeposit = withTests (TestLimit 1) $ property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  poolX       <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range
  epochBudgetInt   <- forAll $ int range

  initLqInt <- forAll $ int range
  afterDepositLqInt <- forAll $ int range

  poolInTxRef    <- forAll genTxOutRef
  let
    epochBudget = toInteger epochBudgetInt

    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = epochNum * epochBudget + 1
    execBudget    = toInteger execBudgetInt
    reserveXInit  = epochNum * epochBudget

    initLq = toInteger initLqInt
    afterDepositLq = (toInteger afterDepositLqInt) + initLq 

    compoundEpoch = 2

    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolDatumToData = toData poolInCfg
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 reserveXInit initLq 0x7fffffffffffffff 0x7fffffffffffffff poolInCfg

    curTimeIdx    = 1


  let curEpochIxRem = curTimeIdx `mod` epochLen

  traceM ("initLq:" ++ show initLq)
  traceM ("afterDepositLq:" ++ show afterDepositLq)
  traceM ("curEpochIxRem:" ++ show curEpochIxRem)

  let curEpochIxR   = curTimeIdx `div` epochLen

  traceM ("curEpochIxR:" ++ show curEpochIxR)

  let curEpochIx    = if (0 < curEpochIxRem) then curEpochIxR + 1 else curEpochIxR 

  traceM ("curEpochIx:" ++ show curEpochIx)

  let curEpochMax   = max 0 curEpochIx

  traceM ("curEpochMax:" ++ show curEpochMax)

  let releasedVLQ = afterDepositLq - initLq

  traceM ("releasedVLQ:" ++ show releasedVLQ)

  let epochsAllocated = epochNum - curEpochMax

  traceM ("epochsAllocated:" ++ show epochsAllocated)

  let releasedTMP     = releasedVLQ * epochsAllocated

  traceM ("releasedTMP:" ++ show releasedTMP)
    --curEpochToCalc = if (curEpochIx <= epochNum) then curEpochIx else epochNum + 1
    --prevEpochsCompoundedForDeposit = ((programBudget - reservesX) + MaxRoundingError0) >= (curEpochToCalc - 1) * epochAlloc

  let
    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 (reserveXInit - epochsAllocated) afterDepositLq (0x7fffffffffffffff - releasedVLQ) (0x7fffffffffffffff - releasedTMP) poolOutCfg

    poolRedeemerToData = toData $ PLM.LMPoolRedeemer 0 0

  traceM ("poolNft:" ++ show poolNft)
  traceM ("poolOutCfg:" ++ show poolOutCfg)
  traceM ("poolTxIn:" ++ show poolTxIn)
  traceM ("poolTxOut:" ++ show poolTxOut)
  let
    txInfo  = mkLMPoolTxInfo [poolTxIn] [poolTxOut] (programStart) (programStart + epochLen + 10)
    purpose = mkPurpose poolInTxRef
    cxtData = toData $ mkContext txInfo purpose

    result = eraseRight $ evalWithArgs (wrapValidator LM.lmPoolValidatorT) [poolDatumToData, poolRedeemerToData, cxtData]
  
  result === Right ()

successRedeem :: Property
successRedeem = withTests (TestLimit 1) $ property $ do
  pkh <- forAll genPkh
  let
    range = exponential 10 512

  poolX       <- forAll genAssetClass
  poolNft     <- forAll genAssetClass
  poolLQAC    <- forAll genAssetClass
  bundleLQAC  <- forAll genAssetClass
  bundleVLQAC <- forAll genAssetClass
  bundleTMPAC <- forAll genAssetClass

  epochLenInt      <- forAll $ int range
  epochNumInt      <- forAll $ int range
  programStartInt  <- forAll $ int range
  programBudgetInt <- forAll $ int range
  execBudgetInt    <- forAll $ int range
  epochBudgetInt   <- forAll $ int range

  initLqInt <- forAll $ int range
  afterDepositLqInt <- forAll $ int range

  poolInTxRef    <- forAll genTxOutRef
  let
    epochBudget = toInteger epochBudgetInt

    epochLen      = toInteger epochLenInt
    epochNum      = (toInteger epochNumInt) + 2
    programStart  = toInteger programStartInt
    programBudget = epochNum * epochBudget + 1
    execBudget    = toInteger execBudgetInt
    reserveXInit  = epochNum * epochBudget

    initLq = toInteger initLqInt
    afterDepositLq = (toInteger afterDepositLqInt) + initLq 

    compoundEpoch = 2

    poolInCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget 1 poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolDatumToData = toData poolInCfg
    poolTxIn  = genPoolTxInInfo poolInTxRef 1 reserveXInit initLq 0x7fffffffffffffff 0x7fffffffffffffff poolInCfg

    curTimeIdx    = 1


  let curEpochIxRem = curTimeIdx `mod` epochLen

  traceM ("initLq:" ++ show initLq)
  traceM ("afterDepositLq:" ++ show afterDepositLq)
  traceM ("curEpochIxRem:" ++ show curEpochIxRem)

  let curEpochIxR   = curTimeIdx `div` epochLen

  traceM ("curEpochIxR:" ++ show curEpochIxR)

  let curEpochIx    = if (0 < curEpochIxRem) then curEpochIxR + 1 else curEpochIxR 

  traceM ("curEpochIx:" ++ show curEpochIx)

  let curEpochMax   = max 0 curEpochIx

  traceM ("curEpochMax:" ++ show curEpochMax)

  let releasedVLQ = afterDepositLq - initLq

  traceM ("releasedVLQ:" ++ show releasedVLQ)

  let epochsAllocated = epochNum - curEpochMax

  traceM ("epochsAllocated:" ++ show epochsAllocated)

  let releasedTMP     = releasedVLQ * epochsAllocated

  traceM ("releasedTMP:" ++ show releasedTMP)
    --curEpochToCalc = if (curEpochIx <= epochNum) then curEpochIx else epochNum + 1
    --prevEpochsCompoundedForDeposit = ((programBudget - reservesX) + MaxRoundingError0) >= (curEpochToCalc - 1) * epochAlloc

  let
    poolOutCfg = genLMPoolConfig epochLen epochNum programStart programBudget execBudget compoundEpoch poolNft poolX poolLQAC bundleVLQAC bundleTMPAC
    poolTxOut = genPoolTxOut 1 (reserveXInit - epochsAllocated) afterDepositLq (0x7fffffffffffffff - releasedVLQ) (0x7fffffffffffffff - releasedTMP) poolOutCfg

    poolRedeemerToData = toData $ PLM.LMPoolRedeemer 0 0

  traceM ("poolNft:" ++ show poolNft)
  traceM ("poolOutCfg:" ++ show poolOutCfg)
  traceM ("poolTxIn:" ++ show poolTxIn)
  traceM ("poolTxOut:" ++ show poolTxOut)
  let
    txInfo  = mkLMPoolTxInfo [poolTxIn] [poolTxOut] (programStart) (programStart + epochLen + 10)
    purpose = mkPurpose poolInTxRef
    cxtData = toData $ mkContext txInfo purpose

    result = eraseRight $ evalWithArgs (wrapValidator LM.lmPoolValidatorT) [poolDatumToData, poolRedeemerToData, cxtData]
  
  result === Right ()