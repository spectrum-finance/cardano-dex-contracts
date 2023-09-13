{-# LANGUAGE OverloadedStrings #-}

module Gen.VestingGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2.Contexts
import PlutusLedgerApi.V1.Time
import PlutusLedgerApi.V1.Interval as PInterval

import qualified ErgoDex.Contracts.Proxy.Vesting as V

genVestingConfig :: Integer -> PubKeyHash -> AssetClass -> V.VestingConfig
genVestingConfig deadline vestingPkh vestingAC =
  V.VestingConfig (POSIXTime deadline) vestingPkh vestingAC

genTxInWithEmptyDatum :: TxOutRef -> Integer -> PubKeyHash -> TxInInfo
genTxInWithEmptyDatum txOutRef adaQty userPkh =
  let
    value = mkValues [mkAdaValue adaQty] mempty
    txOut = mkTxOut' NoOutputDatum value userPkh
  in mkTxIn txOutRef txOut

genVestingTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> TxInInfo
genVestingTxIn txOutRef vestingDatum vestingAC vestingTokenQty =
  let
    value = mkValues [mkValue vestingAC vestingTokenQty, mkAdaValue 1000] mempty
    txOut = mkTxOut vestingDatum value mkVestingValidator
  in mkTxIn txOutRef txOut

genUserTxOut :: AssetClass -> Integer -> PubKeyHash -> TxOut
genUserTxOut vestingAC vestingTokenQty userPkh =
  let
    value = mkValues [mkAdaValue 1000, mkValue vestingAC vestingTokenQty] mempty
  in mkTxOut' NoOutputDatum value userPkh

mkVestingTxInfo :: [TxInInfo] -> [TxOut] -> Integer -> Integer -> PubKeyHash -> TxInfo
mkVestingTxInfo txIns txOuts validRangeStart validRangeEnd userPkh =
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
    , txInfoSignatories = [userPkh]
    , txInfoData = fromList []
    , txInfoId = "b0"
    }