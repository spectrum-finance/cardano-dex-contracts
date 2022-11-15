{-# LANGUAGE OverloadedStrings #-}

module Gen.VestingWithPeriodGen where

import Hedgehog
import Hedgehog.Range
import Hedgehog.Gen

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V1.Time
import PlutusLedgerApi.V1.Interval as PInterval

import ErgoDex.Contracts.Proxy.VestingWithPeriod

genVestingWithPeriodConfig :: Integer -> Integer -> Integer -> Integer -> [PubKeyHash] -> AssetClass -> VestingWithPeriodConfig
genVestingWithPeriodConfig vestingStart vestingPeriodDuration totalVested periodVested signers vestingAC =
  VestingWithPeriodConfig 
    { vestingStart = POSIXTime vestingStart
    , vestingPeriodDuration = POSIXTime vestingPeriodDuration
    , totalVested = totalVested
    , periodVested = periodVested
    , pkhs = signers
    , vestingAC = vestingAC
    }

genVestingWPTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> TxInInfo
genVestingWPTxIn txOutRef vestingDatum vestingAC vestingTokenQty =
  let
    value = mkValues [mkValue vestingAC vestingTokenQty, mkAdaValue 1000] mempty
    txOut = mkTxOut vestingDatum value mkVestingWithPeriodValidator
  in mkTxIn txOutRef txOut

genUserTxOut :: AssetClass -> Integer -> PubKeyHash -> TxOut
genUserTxOut vestingAC vestingTokenQty userPkh =
  let
    value = mkValues [mkValue vestingAC vestingTokenQty, mkAdaValue 1000] mempty
  in mkTxOut' NoOutputDatum value userPkh

genVestingWPTxOut :: OutputDatum -> AssetClass -> Integer -> TxOut
genVestingWPTxOut vestingDatum vestingAC vestingTokenQty =
  let
    value = mkValues [mkValue vestingAC vestingTokenQty, mkAdaValue 1000] mempty
  in mkTxOut vestingDatum value mkVestingWithPeriodValidator

mkVestingTxInfo :: [TxInInfo] -> [TxOut] -> Integer -> Integer -> [PubKeyHash] -> TxInfo
mkVestingTxInfo txIns txOuts validRangeStart validRangeEnd signers =
  let 
    lower = (POSIXTime validRangeStart)
    upper = (POSIXTime validRangeEnd)
  in TxInfo
    { txInfoInputs = txIns
    , txInfoOutputs = txOuts
    , txInfoReferenceInputs = mempty
    , txInfoRedeemers = fromList []
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = PInterval.interval lower upper  --PInterval.Interval lower upper
    , txInfoSignatories = signers
    , txInfoData = fromList []
    , txInfoId = "b0"
    }