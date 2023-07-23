{-# LANGUAGE OverloadedStrings #-}

module Gen.DestroyGen where

import Hedgehog

import Gen.Models
import Gen.DepositGen

import qualified PlutusLedgerApi.V1.Interval as Interval
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2

import qualified ErgoDex.Contracts.Pool as P

genDTxIn :: TxOutRef -> OutputDatum -> AssetClass -> Integer -> AssetClass -> Integer -> Integer -> TxInInfo
genDTxIn ref od lq lqQty nft nftQty adaQty =
  let
    value = mkValues [mkValue lq lqQty, mkValue nft nftQty, mkAdaValue adaQty] mempty
    txOut = mkTxOut od value mkPoolValidator
  in mkTxIn ref txOut

mkDTxInfo :: [TxInInfo] -> TxInfo
mkDTxInfo pIn =
  TxInfo
    { txInfoInputs = pIn
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = fromList []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = fromList []
    , txInfoId = "b0"
    }